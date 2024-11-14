Placeholder readme

usage:

```
use clap::{arg, command, value_parser};
use autorun_config::FromEnv;

#[derive(FromEnv, Debug)]
struct RuntimeConfig {
  uri: String,
  #[env(default = 22)]
  port: i32,
}


fn bootstrap() -> RuntimeConfig {
  let run_args = command!()
    .arg(
      .arg!(--uri <URI>).required(false)
    )
    .arg(
      .arg!(--port <PORT>).required(false).value_parser(value_parser!(i32))
    )
    .get_matches();

  RuntimeConfig::init_from_env(run_args)
}
```

When you define the bootstrap function and pass in the run args, the macro will derive the look ups to find 
the variables declared in the struct. Note that the field in the struct are equal to the "--" arguments. 
The macro will take it's field name and check for <field_name> in the clap arg parser and check for <FIELD_NAME>
in the environment. Right now I only handle strings and i32. There is a default field you can specify as well. 

Preference order:
  If found in command line: Return var
  else: if found in environment: Return var
  else: if default specificed: Return default
  else: panic!()
  
