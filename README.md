# My Dotfiles
This repo houses my personal dotfiles. It should be modular enough to easily be used on a fresh (Ubuntu) machine, but I have not tested this. Currently, it is mostly my Emacs configuration.

# Setup
Install git cli and GNU stow.
```
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y git stow 
```

Clone repo and create symbolic links in home folder.
```
git clone https://github.com/Jameszf/dotfiles.git
cd dotfiles
stow .
```

# TODO List
 - [ ] Add encrypted folder to store sensitive information.
 - [ ] Incorporate personal org-mode files to repo (or separate repo).
 - [ ] Make a Windows install for (atleast) the Emacs config.
 - [ ] Create install script.