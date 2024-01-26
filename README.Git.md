# Git

## Global gitignore:
```
git config --global core.excludesfile $HOME/.gitignore
```

## Reuse Recorded Resolution
```
git config --global rerere.enabled true
```

## Customization

The git global config file (~/.gitconfig) contains the user's name and email, so isn't portable
for inclusion in this repo, however the following can be copied into the file:

```
[color]
	ui = true
[pull]
	rebase = true
	default = current
[push]
	default = current
[alias]
	ls = log
	ci = commit
	st = status
	co = checkout
```
