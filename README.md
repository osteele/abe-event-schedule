# Abvent – ABE Event Display

Display an event schedule from [ABE](https://github.com/olinlibrary/abe).

## Develop

### Install

1. Install [yarn](https://yarnpkg.com).
2. Install npm and elm packages:

    ```bash
    $ yarn install
    ```

### Run

### Run with Hardcoded Data

`yarn start` will run against a hard-coded data set that is compiled into the
program.

### Run against a local ABE instance

1. Install and run the [ABE back end](https://github.com/olinlibrary/abe).
2. Run the following, to seed the database with . Add `--drop` to drop existing events.
    ```bash
    mongoimport --db no-config --collection event --file data/mongodb-example.json
    ```
3. <http://localhost:3000/events/?start=2018-4-15&end=2018-4-16> should show a
   list of events.
4. `yarn start`
5. Visit <http://localhost:4000/?server>. The `?server` query directs the app to
   query the server. (This is the default behavior when the host is not `localhost`
   or `127.0.0.1`.)

### Run against a remote ABE instance

To run against the production server, set the `API_SERVER` environment variable:

```bash
env API_SERVER=https://abe.example.com yarn start
```

### Lint

```bash
$ yarn lint
```

## License

MIT
