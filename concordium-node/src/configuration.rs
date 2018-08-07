use structopt::StructOpt;
use preferences::{AppInfo, PreferencesMap, Preferences};
use std::sync::{Arc,Mutex};
use app_dirs::*;
use std::path::PathBuf;

const APP_INFO: AppInfo = AppInfo{name: "ConcordiumP2P", author: "Concordium"};
const APP_PREFERENCES_MAIN:&str = "main/config";
const APP_PREFERENCES_KEY_VERSION:&str = "VERSION";

#[derive(StructOpt, Debug)]
#[structopt(name = "config")]
pub struct Config {
    #[structopt(long="no-network", short="nonet", help = "Disable network")]
    pub no_network: bool,
    #[structopt(long="connect-to", short="c", help = "Peer to connect to upon startup")]
    pub connect_to: Option<String>,
    #[structopt(long="listen-port", short="l", help = "Port to listen on")]
    pub listen_port: Option<u16>,
    #[structopt(long="id", short="i", help = "Wanted ID")]
    pub id: Option<String>,
    #[structopt(long="debug", help = "Debug mode")]
    pub debug: bool,
    #[structopt(long="no-rpc-server", help ="Disable the built-in RPC server")]
    pub no_rpc_server: bool,
    #[structopt(long="rpc-server-port", help = "RPC server port", default_value="10000")]
    pub rpc_server_port: u16,
    #[structopt(long="rpc-server-addr", help = "RPC server listen address", default_value="127.0.0.1")]
    pub rpc_server_addr: String,
    #[structopt(long="rpc-server-token", help = "RPC server access token")]
    pub rpc_server_token: Option<String>
}

pub fn parse_config() -> Config  {
    return Config::from_args();
}

pub struct AppPreferences {
    preferences_map: Arc<Mutex<PreferencesMap<String>>>
}

impl AppPreferences {
    pub fn new() -> Self {
        let load_result = PreferencesMap::<String>::load(&APP_INFO, APP_PREFERENCES_MAIN);
        if load_result.is_ok() {
            let mut prefs = load_result.unwrap();
            if !prefs.contains_key(&APP_PREFERENCES_KEY_VERSION.to_string()) {
                prefs.insert(APP_PREFERENCES_KEY_VERSION.to_string(), super::VERSION.to_string() );
                if !prefs.save(&APP_INFO, APP_PREFERENCES_MAIN).is_ok() {
                    panic!( "Can't write to config file!");
                }
            } else if *prefs.get(&APP_PREFERENCES_KEY_VERSION.to_string()).unwrap() != super::VERSION.to_string() {
                panic!("Incorrect version of config file!");
            }
            AppPreferences {
                preferences_map: Arc::new(Mutex::new(prefs)),
            }
        } else {
            let mut prefs = PreferencesMap::<String>::new();
            prefs.insert(APP_PREFERENCES_KEY_VERSION.to_string(), super::VERSION.to_string() );
            if !prefs.save(&APP_INFO, APP_PREFERENCES_MAIN).is_ok() {
                panic!( "Can't write to config file!");
            }
            AppPreferences {
                preferences_map: Arc::new(Mutex::new(prefs)),
            }
        }
    }

    pub fn set_config( &mut self, key: String, value: Option<String> ) -> bool {
        if let Ok(ref mut store) = self.preferences_map.try_lock() {
            match value {
                Some(val) => {
                    store.insert(key, val);
                },
                _ => {
                    store.remove(&key);
                }
            }
            store.save(&APP_INFO, APP_PREFERENCES_MAIN).is_ok()
        } else {
            false
        }
    }

    pub fn get_config( &self, key: String) -> Option<String> {
        match self.preferences_map.lock().unwrap().get(&key)  {
            Some(res) => Some(res.clone()),
            _ => None
        }
    }

    pub fn get_user_app_dir(&self) -> PathBuf {
        get_app_root(AppDataType::UserData, &APP_INFO).unwrap()
    }

    pub fn get_user_config_dir(&self) -> PathBuf {
        get_app_root(AppDataType::UserConfig, &APP_INFO).unwrap()
    }
}