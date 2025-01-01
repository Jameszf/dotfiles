# My Dotfiles
These are my dotfiles. It is primarily just my Emacs configuration (since that is what I use the most), but I may eventually add other configurations for other programs I use. 

# Setup
The following setup instructions should work for Debian distros.

Install git cli and GNU stow.
```
sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y git stow
```

Clone repo and create symbolic links in home folder.
```
cd ~
git clone https://github.com/Jameszf/dotfiles.git
cd dotfiles
stow .
```

## Emacs
There are several things you need to do for my Emacs configuration to work:

1. Install Emacs dependencies.
```
sudo apt-get install -y sqlite3
```

2. Create `org/` and `.saves/` folders to `.emacs.d/`.
```
mkdir dotfiles/.emacs.d/org
mkdir dotfiles/.emacs.d/.saves
```

3. Download and add Office Code Pro 12 to `.fonts` in home directory. OTF files should work.


# TODO List
 - [ ] Add encrypted folder to store sensitive information.
 - [ ] Incorporate personal org-mode files to repo (or separate repo).
 - [ ] Make a Windows install for (atleast) the Emacs config.
 - [ ] Create install script.
 - [ ] Ensure Emacs config works on fresh Windows/Linux installs.