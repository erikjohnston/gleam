extern crate gleam;
extern crate hyper;
#[macro_use]
extern crate gleam_derive;
extern crate futures;
extern crate anymap;
#[macro_use]
extern crate slog;
extern crate slog_async;
extern crate slog_term;

use gleam::{Ctx, Server};
use hyper::server::Response;
use hyper::{Method, StatusCode};
use hyper::header::ContentType;
use slog::Drain;


#[derive(GleamState)]
struct AppState {
    the_string: String,
}


fn render_test_path(_: Ctx, state: AppState, req: TestRequest) -> Result<Response, ()> {
    let body = format!(
        "{}\n{:?}\n{}\n{}\n",
        req.header_host,
        req.header_x_forwarded_proto,
        req.path_name,
        state.the_string,
    );

    let resp = Response::new()
        .with_header(ContentType::plaintext())
        .with_status(StatusCode::Ok)
        .with_body(body);

    Ok(resp)
}


#[derive(GleamFromRequest)]
struct TestRequest {
    header_host: String,
    header_x_forwarded_proto: Option<String>,
    path_name: u8,
}


fn main() {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let logger = slog::Logger::root(drain, o!());

    let mut server = Server::with_logger(logger);

    server.manage_state(String::from("The String"));

    server.add_route(Method::Get, "/test/:name", render_test_path);

    let addr = String::from("0.0.0.0:7878").parse().unwrap();
    server.run(&addr).unwrap()
}

