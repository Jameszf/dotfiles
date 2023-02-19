# My Dotfiles
These are my dotfiles. It is primarily just my Emacs configuration (since that is what I use the most), but I may eventually add other configurations for other programs I use.

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
 - [ ] Ensure Emacs config works on fresh Windows/Linux installs.