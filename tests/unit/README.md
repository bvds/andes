# Workspace API Test Suite

This suite contains thorough BDD tests which aim to cover all functionalities, as well as security for all available API calls.

## Run

To run the test, simply open this directory on a browser. Node.js and CI support will be added soon.

## Writing Specs

[Mocha](http://visionmedia.github.io/mocha/) is used as the test framework.

Specs are located inside the `specs` folder. Each file represents a service end-point.

To include the spec file into the runner, simply add it to `specs.txt`. It's just a text file,
each spec is simply separated by a new line. The file order is exactly the same as the order of execution, from top to bottom.

To comment out a spec file, simply add a "#" in front of the line.

## Changing Server URL

Open `index.html` and change the argument that is passed to the `run()` method

	run('http://80550f113a2347519bc654c91bc2887d.cloudapp.net/json.rpc');

## Error Response Format

An error should always have this format:

	{
		"code": 12345,
		"param": "PARAM_NAME",
		"message": "VERBOSE_ERROR_MESSAGE"
	}

For standard errors, `code` follows the convention [here](http://www.jsonrpc.org/specification#error_object). We pick `-32001` as the common error code when a given param is invalid.

`param` indicates the parameter to which the error is associated. This value is used on the client app to highlight the corresponding input. For example, an invalid email address when signing in may have an error response of:

	{
        "code": -32001,
        "param": "email",
        "message": "Incorrect email address"
    }

`param` can simply be omitted if the error is not associated to a specific parameter.

`message` should be clear and concise. This exact string is displayed on the UI dialog for the end-user.

## SMD

The full SMD for reference is named `smd.json`. This file should always be kept up-to-date whenever there's any changes. All specs are written (and generated) based on this file.

## Setup / Teardown Process

Given that `{TEST_SERVER_URL}` is the URL to the test server (e.g `http://80550f113a2347519bc654c91bc2887d.cloudapp.net/json.rpc`)

To setup a session, the runner makes a GET request to `{TEST_SERVER_URL}?setup`. The server creates a fresh, empty test database, associated with a unique session id. The response body will then simply be that session id.

All API calls from the specs will then point to `{TEST_SERVER_URL}?id={SESSION_ID}`

When then suite ends, a GET request is made to `{TEST_SERVER_URL}?teardown&id={SESSION_ID}`. A timeout will need to be put in place to automatically wipe out the test database after the certain period, if the client never initiates the teardown request for some reasons.




