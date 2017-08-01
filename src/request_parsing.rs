use std::str;
use std::str::FromStr;
use std::borrow::Cow;
use std::convert::From;

use hyper::server::{Request, Response};
use hyper::StatusCode;
use hyper::header::ContentType;
use linear_map::LinearMap;
use url::form_urlencoded;


use IntoResponse;


pub struct BadRequest(pub String);

impl IntoResponse for BadRequest {
    fn into_response(self) -> Response {
        Response::new()
            .with_header(ContentType::plaintext())
            .with_status(StatusCode::BadRequest)
            .with_body(self.0)
    }
}

impl From<String> for BadRequest {
    fn from(s: String) -> BadRequest {
        BadRequest(s)
    }
}


impl From<BadRequest> for Response {
    fn from(r: BadRequest) -> Response {
        r.into_response()
    }
}



pub trait FromHttpStr: Sized {
    fn parse_from_http(s: &str) -> Result<Self, BadRequest>;
}



pub trait FromHeader: Sized {
    fn parse_header<'a, I: Iterator<Item = &'a [u8]>>(
        name: &'a str,
        it: I,
    ) -> Result<Self, BadRequest>;
}

pub trait FromHeaderLine: Sized {
    fn parse_header_line<'a>(
        name: &'a str,
        line: &'a [u8],
    ) -> Result<Self, BadRequest>;
}

impl<T> FromHeaderLine for T
where
    T: FromHttpStr,
{
    fn parse_header_line<'a>(
        name: &'a str,
        line: &'a [u8],
    ) -> Result<Self, BadRequest> {
        let header = str::from_utf8(line)
            .map_err(|_| format!("Header '{}' not UTF-8", name))?;
        let header = T::parse_from_http(header)
            .map_err(|e| format!("Bad header '{}': {}", name, e.0))?;
        Ok(header)
    }
}

macro_rules! derive_from_from_str {
    ($type_name:ident) => {
        impl FromHttpStr for $type_name {
            fn parse_from_http(s: &str) -> Result<Self, BadRequest>
            {
                $type_name::from_str(s).map_err(
                    |e| format!("{}", e)
                )
                .map_err(BadRequest::from)
            }
        }
    }
}

derive_from_from_str!{ String }
derive_from_from_str!{ bool }
derive_from_from_str!{ u8 }
derive_from_from_str!{ u16 }
derive_from_from_str!{ u32 }
derive_from_from_str!{ u64 }
derive_from_from_str!{ usize }
derive_from_from_str!{ i8 }
derive_from_from_str!{ i16 }
derive_from_from_str!{ i32 }
derive_from_from_str!{ isize }
derive_from_from_str!{ f32 }
derive_from_from_str!{ f64 }



impl<T> FromHeader for Option<T>
where
    T: FromHeaderLine,
{
    fn parse_header<'a, I: Iterator<Item = &'a [u8]>>(
        name: &'a str,
        it: I,
    ) -> Result<Self, BadRequest> {
        match it.last() {
            Some(buf) => Ok(Some(T::parse_header_line(name, buf)?)),
            None => Ok(None),
        }
    }
}

impl<T> FromHeader for Vec<T>
where
    T: FromHeaderLine,
{
    fn parse_header<'a, I: Iterator<Item = &'a [u8]>>(
        name: &'a str,
        it: I,
    ) -> Result<Self, BadRequest> {
        it.map(|line| T::parse_header_line(name, line)).collect()
    }
}


impl<T> FromHeader for T
where
    T: FromHeaderLine,
{
    fn parse_header<'a, I: Iterator<Item = &'a [u8]>>(
        name: &'a str,
        it: I,
    ) -> Result<Self, BadRequest> {
        match it.last() {
            Some(buf) => Ok(T::parse_header_line(name, buf)?),
            None => Err(format!("Missing required header '{}'", name)),
        }.map_err(BadRequest::from)
    }
}


pub trait FromPathParam: Sized {
    fn parse_path_param<'a>(
        name: &'a str,
        params: &'a ::Params,
    ) -> Result<Self, BadRequest>;
}

impl<T> FromPathParam for Option<T>
where
    T: FromHttpStr,
{
    fn parse_path_param<'a>(
        name: &'a str,
        params: &'a ::Params,
    ) -> Result<Self, BadRequest> {
        if let Some(s) = params.find(name) {
            Ok(Some(
                T::parse_from_http(s)
                    .map_err(
                        |e| format!("Bad path segment '{}': {}", name, e.0),
                    )?,
            ))
        } else {
            Ok(None)
        }
    }
}


impl<T> FromPathParam for T
where
    T: FromHttpStr,
{
    fn parse_path_param<'a>(
        name: &'a str,
        params: &'a ::Params,
    ) -> Result<Self, BadRequest> {
        if let Some(s) = params.find(name) {
            Ok(
                T::parse_from_http(s)
                    .map_err(
                        |e| format!("Bad path segment '{}': {}", name, e.0),
                    )?,
            )
        } else {
            Err(BadRequest(format!("Missing path segment '{}'", name)))
        }
    }
}


pub fn parse_query_string<'a>(
    req: &'a Request,
) -> LinearMap<Cow<'a, str>, Cow<'a, str>> {
    let query = if let Some(query) = req.query() {
        query
    } else {
        return LinearMap::new();
    };

    form_urlencoded::parse(query.as_bytes()).collect()
}


pub trait FromQueryParam: Sized {
    fn parse_query_param<'a>(
        name: &'a str,
        req: &LinearMap<Cow<'a, str>, Cow<'a, str>>,
    ) -> Result<Self, BadRequest>;
}


impl<T> FromQueryParam for Option<T>
where
    T: FromHttpStr,
{
    fn parse_query_param<'a>(
        name: &'a str,
        map: &LinearMap<Cow<'a, str>, Cow<'a, str>>,
    ) -> Result<Self, BadRequest> {
        if let Some(s) = map.get(name) {
            Ok(
                Some(
                    T::parse_from_http(s)
                        .map_err(
                            |e| format!("Bad query param '{}': {}", name, e.0),
                        )?,
                ),
            )
        } else {
            Ok(None)
        }
    }
}

impl<T> FromQueryParam for T
where
    T: FromHttpStr,
{
    fn parse_query_param<'a>(
        name: &'a str,
        map: &LinearMap<Cow<'a, str>, Cow<'a, str>>,
    ) -> Result<Self, BadRequest> {
        if let Some(s) = map.get(name) {
            Ok(
                T::parse_from_http(s)
                    .map_err(|e| format!("Bad query param '{}': {}", name, e.0))?,
            )
        } else {
            Err(format!("Missing query param '{}'", name))
        }
        .map_err(BadRequest::from)
    }
}
