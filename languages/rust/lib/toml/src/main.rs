use serde::Deserialize;
use toml;

static config_toml: &str = r#"
[settings]
name = "comp"
verbose = "true"
"#;

#[derive(Deserialize)]
struct Config {
    settings: Settings,
}

#[derive(Deserialize)]
struct Settings {
    name: String,
    verbose: String,
}

fn main() {
    println!("\nconfig_toml: {}", config_toml);

    let config: Config = toml::from_str(config_toml).unwrap();

    println!("config.settings.name = {}", config.settings.name);
    println!("config.settings.verbose = {}", config.settings.verbose);
}
