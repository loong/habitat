#!/usr/bin/env sh

# fnm, install if not there
if ! which fnm > /dev/null; then
   echo "fnm not found! Installing..."
   curl -fsSL https://fnm.vercel.app/install | zsh
fi

eval "$(fnm env --use-on-cd)"
fnm install 18.17.1

npm install -g typescript-language-server typescript
npm i -g bash-language-server
