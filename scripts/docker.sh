#!/bin/bash

# zsh completion directory
mkdir -p ~/.zsh/completion

# Docker compose completions
curl -L https://raw.githubusercontent.com/docker/compose/master/contrib/completion/zsh/_docker-compose > ~/.zsh/completion/_docker-compose

# docker completions
curl -L https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/zsh/_docker > ~/.zsh/completion/_docker
