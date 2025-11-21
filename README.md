# Cube Solver - Server
Uses the [cube solver](https://github.com/voiestad/cube-solver) and starts an API that takes a scramble and gives a PDF solution as Base64 response.

## How to run
### Cabal
```
cabal run
```
### Docker
```
docker build -t voiestad/cube-solver-server .
```
```
docker run -p 8080:8080 voiestad/cube-solver-server
```

## API
To interact with the API, send a GET request to `https://voiestad.no/api/cubesolver` with a parameter named `cube` containing a scrambled cube in the same format as described [here](https://github.com/voiestad/cube-solver#scramble-input-example-file).<br>
You can also use the [web UI](https://voiestad.no/cube-solver) to interact with the API.

### API endpoints
- **Validate**: Returns `true` if the provided scramble is valid, `false` otherwise.
- **Solve**: Returns the solution for the provided scramble as PDF.

### Example API call - Validate
```
https://voiestad.no/api/cubesolver/validate?cube=YBRRWROYBGYGWBOWGWGWROOGOGRWRGYBWGYOYWWBOBYBOBGROYBYRR
```

### Example API call - Solve
```
https://voiestad.no/api/cubesolver/solve?cube=YBRRWROYBGYGWBOWGWGWROOGOGRWRGYBWGYOYWWBOBYBOBGROYBYRR
```

## Credit
- [GAN Bluetooth with Node.js by Andy Fedotov](https://github.com/afedotov/gan-node-sample)
