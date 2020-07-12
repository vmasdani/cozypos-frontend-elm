# CozyPoS Frontend Elm

Rewrite of `cozypos-frontend` formerly from Vue, to Elm.

## Compiling
1. Install `elm`
2. Compile
```bash
elm make src/Main.elm --output=main.js
# or
sudo chmod +x ./start.sh
./start.sh
# To optimize:
sudo chmod +x ./optimize.sh
sudo npm i -g uglify-js
./optimize.sh
```
3. Serve with simple HTTP Server
```bash
python3 -m http.server
```
