#!/bin/bash

# Install EMACS 24
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot emacs-snapshot-el

# Install tmux
sudo apt-get install tmux
