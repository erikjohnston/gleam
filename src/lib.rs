extern crate anymap;
extern crate futures;
extern crate hyper;
extern crate route_recognizer;
extern crate url;
#[macro_use]
extern crate lazy_static;
extern crate linear_map;
#[macro_use]
extern crate slog;
extern crate slog_stdlog;
extern crate tokio_core;


use hyper::server::{Request, Response, Service};
use hyper::StatusCode;
use hyper::header::ContentType;
use hyper::Method;
use futures::{Future, IntoFuture, Stream};
use route_recognizer::{Match, Router};
use slog::Drain;
use linear_map::LinearMap;
use tokio_core::reactor::Core;
use tokio_core::net::TcpListener;

use std::net::SocketAddr;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::cell::RefCell;

pub use route_recognizer::Params;

pub mod request_parsing;
pub use request_parsing::BadRequest;


lazy_static! {
    static ref NEXT_REQUEST_ID: AtomicUsize = AtomicUsize::new(1);
}


pub struct Server {
    router_map: LinearMap<Method, Router<Box<CallFromRequest>>>,
    state_map: anymap::Map<anymap::any::Any>,
    logger: slog::Logger,
}

impl Server {
    pub fn new() -> Server {
        Server {
            router_map: LinearMap::new(),
            state_map: anymap::Map::new(),
            logger: slog::Logger::root(
                slog_stdlog::StdLog.fuse(), o!()
            ),
        }
    }

    pub fn with_logger(logger: slog::Logger) -> Server {
        Server {
            router_map: LinearMap::new(),
            state_map: anymap::Map::new(),
            logger: logger,
        }
    }

    pub fn add_route<S, T, F, R, H>(&mut self, method: Method, path: &str, f: F)
        -> &mut Server
    where
        F: Fn(Ctx, S, T) -> R + 'static,
        S: State + 'static,
        T: FromRequest + 'static,
        R: IntoFuture<Item = H> + 'static,
        T::Future: 'static,
        R::Error: IntoResponse,
        R::Future: 'static,
        H: IntoResponse + 'static,
    {
        let wrapped = FuncationCall::<S, T, F, R>::new(f);

        self.router_map
            .entry(method)
            .or_insert_with(Router::new)
            .add(path, Box::new(wrapped));

        info!(self.logger, "Registered path: {}", path);

        self
    }

    pub fn add_route_with_body<S, T, B, F, R, H>(&mut self, method: Method, path: &str, f: F)
        -> &mut Server
    where
        F: Fn(Ctx, S, T, B) -> R + 'static,
        S: State + 'static,
        B: IntoBody + 'static,
        T: FromRequest + 'static,
        R: IntoFuture<Item = H> + 'static,
        T::Future: 'static,
        R::Error: IntoResponse,
        R::Future: 'static,
        H: IntoResponse + 'static,
    {
        let wrapped = FuncationCallBody::<S, T, B, F, R>::new(f);

         self.router_map
            .entry(method)
            .or_insert_with(Router::new)
            .add(path, Box::new(wrapped));

        info!(self.logger, "Registered path with body: {}", path);

        self
    }

    pub fn manage_state<T>(&mut self, state: T) -> &mut Server
    where
        T: 'static,
    {
        let prev_value = self.state_map.insert(state);
        if prev_value.is_some() {
            panic!("Trying to add state of a type that already exists");
        }

        self
    }

    pub fn get_state<T>(&self) -> Option<&T>
    where
        T: 'static,
    {
        self.state_map.get()
    }

    pub fn run(mut self, addr: &SocketAddr) -> Result<(), hyper::Error> {
        let mut core = Core::new()?;
        let handle = core.handle();
        let listener = TcpListener::bind(&addr, &handle)?;

        self.manage_state(core.handle());

        let protocol = hyper::server::Http::new();

        info!(self.logger, "Server started");

        let server_arc = Rc::new(self);
        let srv = listener.incoming().for_each(|(socket, addr)| {
            protocol.bind_connection(&handle, socket, addr, server_arc.clone());
            Ok(())
        });

        core.run(srv).map_err(|e| e.into())
    }
}

impl Service for Server {
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error>>;

    fn call(&self, req: Request) -> Self::Future {
        let request_id = NEXT_REQUEST_ID.fetch_add(1, Ordering::Relaxed);

        let logger = self.logger.new(o!(
            "request_id" => request_id,
            "method" => format!("{}", req.method()),
            "path" => format!("{}", req.path()),
        ));

        let ctx = Ctx {
            request_id: request_id,
            logger: Rc::new(RefCell::new(logger)),
        };

        debug!(ctx, "Received request");

        let ctx2 = ctx.clone();
        let log_resp = move |r: Result<Response, _>| {
            match r {
                Ok(ref resp) => info!(
                    ctx2, "Processed request";
                    "status" => u16::from(resp.status())
                ),
                Err(_) => error!(
                    ctx2, "Failed to process request"
                ),
            }
            r
        };

        if let Some(m) = self.router_map
            .get(req.method())
            .and_then(|router| router.recognize(req.uri().path()).ok())
        {
            let Match { handler, params } = m;
            Box::new(
                handler.call(&self, ctx, req, params)
                    .then(log_resp)
            )
        } else {
            Box::new(
                ::futures::future::ok(
                    Response::new()
                        .with_header(ContentType::html())
                        .with_status(StatusCode::NotFound),
                )
                .then(log_resp)
            )
        }
    }
}

#[derive(Clone)]
pub struct Ctx {
    request_id: usize,
    logger: Rc<RefCell<slog::Logger>>,
}

impl Ctx {
    pub fn update_logger<T>(&mut self, values: slog::OwnedKV<T>)
    where T: slog::SendSyncRefUnwindSafeKV + 'static,
    {
        let new_logger = self.logger.borrow().new(values);
        *self.logger.borrow_mut() = new_logger;
    }

    pub fn logger(&self) -> &RefCell<slog::Logger> {
        &self.logger
    }

    pub fn log(&self, record: &slog::Record) {
        self.logger.borrow().log(record);
    }

    pub fn request_id(&self) -> usize {
        self.request_id
    }
}


pub trait IntoResponse: Sized {
    fn into_response(self) -> hyper::Response;
}


impl IntoResponse for () {
    fn into_response(self) -> hyper::Response {
        Response::new()
            .with_header(ContentType::plaintext())
            .with_status(StatusCode::InternalServerError)
            .with_body("Internal Server Error\n")
    }
}


impl IntoResponse for String {
    fn into_response(self) -> hyper::Response {
        Response::new()
            .with_header(ContentType::plaintext())
            .with_status(StatusCode::InternalServerError)
            .with_body(self)
    }
}

impl IntoResponse for &'static str {
    fn into_response(self) -> hyper::Response {
        Response::new()
            .with_header(ContentType::plaintext())
            .with_status(StatusCode::InternalServerError)
            .with_body(self)
    }
}

impl IntoResponse for hyper::Response {
    fn into_response(self) -> hyper::Response {
        self
    }
}


pub trait CallFromRequest {
    fn call<'a>(
        &self,
        &Server,
        Ctx,
        Request,
        Params,
    ) -> Box<Future<Item = Response, Error = hyper::Error>>;
}

pub trait FromRequest: Sized {
    type Error: IntoResponse;
    type Future: IntoFuture<Item = Self, Error = Self::Error>;

    fn from_request(&Server, &Ctx, &Request, &Params) -> Self::Future;
}

pub trait IntoBody: Sized {
    type Error: IntoResponse;
    type Future: IntoFuture<Item = Self, Error = Self::Error>;

    fn into_body(&Server, &Ctx, Request, Params) -> Self::Future;
}

impl IntoBody for hyper::Body {
    type Error = ();
    type Future = Result<hyper::Body, Self::Error>;

    fn into_body(_: &Server, _: &Ctx, req: Request, _: Params) -> Self::Future {
        Ok(req.body())
    }
}


pub trait State: Sized {
    type Error: IntoResponse;

    fn get_state(&Server) -> Result<Self, Self::Error>;
}

impl State for () {
    type Error = ();

    fn get_state(_: &Server) -> Result<Self, Self::Error> {
        Ok(())
    }
}


impl FromRequest for () {
    type Error = &'static str;
    type Future = Result<Self, Self::Error>;

    fn from_request(_: &Server, _: &Ctx, _: &Request, _: &Params) -> Self::Future {
        Ok(())
    }
}

struct FuncationCall<S, T, F, R>
where
    F: Fn(Ctx, S, T) -> R,
    T: FromRequest,
{
    f: Rc<F>,
    t: std::marker::PhantomData<(S, T)>,
}

impl<S, T, F, R> FuncationCall<S, T, F, R>
where
    F: Fn(Ctx, S, T) -> R + 'static,
    T: FromRequest,
{
    pub fn new(f: F) -> FuncationCall<S, T, F, R> {
        FuncationCall {
            f: Rc::new(f),
            t: std::marker::PhantomData,
        }
    }
}


impl<S, T, F, R, H> CallFromRequest for FuncationCall<S, T, F, R>
where
    F: Fn(Ctx, S, T) -> R + 'static,
    T: FromRequest,
    T::Future: 'static,
    S: State + 'static,
    R: IntoFuture<Item = H>,
    R::Error: IntoResponse,
    R::Future: 'static,
    H: IntoResponse + 'static,
{
    fn call<'a>(
        &self,
        server: &Server,
        ctx: Ctx,
        req: Request,
        params: Params,
    ) -> Box<Future<Item = Response, Error = hyper::Error>> {
        let func = self.f.clone();

        let fut = T::from_request(server, &ctx, &req, &params)
            .into_future()
            .map_err(move |e| e.into_response())
            .join(S::get_state(server).map_err(move |e| e.into_response()))
            .and_then(move |(r, state)| {
                func(ctx, state, r)
                    .into_future()
                    .map(|r| r.into_response())
                    .or_else(move |e| Ok(e.into_response()))
            })
            .or_else(move |e| Ok(e));
        Box::new(fut)
    }
}


struct FuncationCallBody<S, T, B, F, R>
where
    F: Fn(Ctx, S, T, B) -> R,
    T: FromRequest,
    B: IntoBody,
{
    f: Rc<F>,
    t: std::marker::PhantomData<(S, T, B)>,
}

impl<S, T, B, F, R> FuncationCallBody<S, T, B, F, R>
where
    F: Fn(Ctx, S, T, B) -> R + 'static,
    T: FromRequest,
    B: IntoBody,
{
    pub fn new(f: F) -> FuncationCallBody<S, T, B, F, R> {
        FuncationCallBody {
            f: Rc::new(f),
            t: std::marker::PhantomData,
        }
    }
}


impl<S, T, B, F, R, H> CallFromRequest for FuncationCallBody<S, T, B, F, R>
where
    F: Fn(Ctx, S, T, B) -> R + 'static,
    T: FromRequest + 'static,
    B: IntoBody,
    T::Future: 'static,
    B::Future: 'static,
    S: State + 'static,
    R: IntoFuture<Item = H>,
    R::Error: IntoResponse,
    R::Future: 'static,
    H: IntoResponse + 'static,
{
    fn call<'a>(
        &self,
        server: &Server,
        ctx: Ctx,
        req: Request,
        params: Params,
    ) -> Box<Future<Item = Response, Error = hyper::Error>> {
        let func = self.f.clone();

        let fut = T::from_request(server, &ctx, &req, &params);
        let body_fut = B::into_body(server, &ctx, req, params);

        let fut = fut
            .into_future()
            .map_err(move |e| e.into_response())
            .join(S::get_state(server).map_err(move |e| e.into_response()))
            .and_then(move |(r, state)| {
                body_fut
                    .into_future()
                    .map_err(move |e| e.into_response())
                    .and_then(move |body| {
                        func(ctx, state, r, body)
                            .into_future()
                            .map(|r| r.into_response())
                            .or_else(move |e| Ok(e.into_response()))
                    })
            })
            .or_else(move |e| Ok(e));

        Box::new(fut)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;
    use std::iter::empty;
    use request_parsing::FromHeader;

    pub struct TestRequest {
        pub header_x_forwarded_proto: Option<String>,
    }

    impl FromRequest for TestRequest {
        type Error = BadRequest;
        type Future = Result<Self, BadRequest>;

        fn from_request(
            _: &Server,
            _: &Ctx,
            req: &Request,
            _: &Params,
        ) -> Result<TestRequest, Self::Error> {
            let header_x_forwarded_proto = {
                let raw_header = req.headers().get_raw("x-forwarded-proto");
                if let Some(hdr) = raw_header {
                    <Option<String> as FromHeader>::parse_header(
                        "x-forwarded-proto",
                        hdr.iter(),
                    )?
                } else {
                    <Option<String> as FromHeader>::parse_header(
                        "x-forwarded-proto",
                        empty(),
                    )?
                }
            };

            Ok(TestRequest {
                header_x_forwarded_proto,
            })
        }
    }

    #[test]
    fn test() {
        let mut server = Server::new();
        server.add_route(
            hyper::Method::Get,
            "/test",
            |_, _state: (), _req: TestRequest| {
                ::futures::future::ok::<_, ()>(
                    Response::new()
                        .with_header(ContentType::html())
                        .with_status(StatusCode::Ok),
                ).boxed()
            },
        );

        let mut req = Request::new(
            hyper::Method::Get,
            hyper::Uri::from_str("/test").unwrap(),
        );

        req.headers_mut().set_raw("X-Forwarded-Proto", "https");

        let resp = server.call(req).wait().unwrap();

        assert_eq!(resp.status(), StatusCode::Ok);
    }
}
