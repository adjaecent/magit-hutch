struct Config {
    host: String,
    port: u16,
}

impl Config {
    fn new(host: &str, port: u16) -> Self {
        Config {
            host: host.to_string(),
            port,
        }
    }

    fn addr(&self) -> String {
        format!("{}:{}", self.host, self.port)
    }
}

fn process(items: &[i32]) -> i32 {
    items.iter().sum()
}

fn main() {
    let cfg = Config::new("localhost", 8080);
    println!("{}", cfg.addr());
}
