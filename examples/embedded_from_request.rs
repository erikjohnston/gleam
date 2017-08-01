extern crate gleam;
extern crate hyper;
#[macro_use]
extern crate gleam_derive;
extern crate futures;
extern crate anymap;

use gleam::{Ctx, Server};
use hyper::server::Response;
use hyper::{Method, StatusCode};
use hyper::header::ContentType;


#[derive(GleamState)]
struct AppState {
    the_string: String,
}


#[derive(GleamFromRequest)]
struct TestRequestEmbed {
    path_bar: u8,
}


#[derive(GleamFromRequest)]
struct TestRequest {
    path_foo: u8,
    req_bar: TestRequestEmbed,
}


fn render_test_path(_: Ctx, state: AppState, req: TestRequest) -> Result<Response, ()> {
    let body = format!(
        "{}\n{}\n{}\n",
        req.path_foo,
        req.req_bar.path_bar,
        state.the_string,
    );

    let resp = Response::new()
        .with_header(ContentType::plaintext())
        .with_status(StatusCode::Ok)
        .with_body(body);

    Ok(resp)
}


fn main() {
    let mut server = Server::new();
    server.manage_state(String::from("The String"));

    server.add_route(Method::Get, "/test/:name", render_test_path);

    let addr = String::from("0.0.0.0:7878").parse().unwrap();
    server.run(&addr).unwrap()
}
