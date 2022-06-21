# Simple news server

News server for metalamp internship.

## Setting up and running
1. Download and install `stack` as described [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
2. Clone this repository as described [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) or with `git clone https://github.com/alleksandrgall/MynewsServer` command if you have `git` installed.
3. `stack setup` for downloading compiler, 
   `stack build` for building app and downloading dependencies.
4. Make a `.cfg` file, or use the template `MynewsServer/config/config_template.cfg` (there you can also find what can be configured).
5. Setup your local PostgreSQL database and copy a connection string to your config file.
6. Provide a folder where images will be stored and put it's path to the config file. 
7. Populate server database by typing `stack run %YOUR_CONFIG_PATH% --migrate`.
8. Run the server by typing `stack run %YOUR_CONFIG_PATH%`

## Project structer
* `app/Main` - Parses command arguments and runs server logic with production handlers.
* `curl` - Curl request for testing the server
* `src/Handlers` - Basic logic for the server, intoduces App monad, DB scheme and logger.
  * `src/Handlers/App` - App monad (a wrapper over Servant Handler monad with access to DB, Logger, Image managment etc.
  * `src/Handlers/DB` - DB scheme, DB handler (how to run Persistent eDSL), inital migration logic
  * `src/Handlers/Image` - Logic of saving, deleting and getting an image from a local storage safely.
  * `src/Handlers/Katip` - Wrapper around Katip environment used to implement KatipMiddleware for logging raw requests.
* `src/Config.hs` - Parser for Configurator's config.
* `src/App/Prod.hs` - App implementation depending on Configurator's config.
* `src/DB/Postgres.hs` - DB implementation for PostgreSQL database.
* `src/Image/File.hs` - Image storage implementation for storing images in file system.
* `src/Api` - Server API implemented in Servant
* `test` - Tests for application
