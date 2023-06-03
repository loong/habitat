#!/bin/bash
brew install pyenv
pyenv install 3.10.7
pyenv global 3.10.7

echo -e 'if command -v pyenv 1>/dev/null 2>&1; then\n  eval "$(pyenv init -)"\nfi' >> ~/.zshrc
