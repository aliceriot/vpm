# vim plugin manager

## key design goals

- No changes to `.vimrc` or `init.vim` necessary to add and use plugins
- optionally use a lockfile to enable dealing with incompatibilities
  between plugins and reproducible installs more easily
- provide a set of rules for lazily loading packages
- package management is done from the command line, as little
  functionality implemented in Vimscript as possible
- pleasant API for adding and upgrading packages, setting rules, etc
- alert the user if a new version of `vpm` is available

### nice to haves

- save somewhere (maybe just in a big JSON file) a database of default
  values for plugin filetypes, postinstall commands, whether they are nvim
  only, etc

## sketch of functionality:

### startup / install

I'm planning to write the thing in Nodejs, and publish it on `npm`, so
a user shouldn't need to do anything other than:

```
npm install -g vim-package-manager
```

this provides an executable on the users `$PATH` called `vpm`.

```
vpm init
```

initialize a new vim plugin configuration in the current directory. this will:

- detect if the `cwd` is inside `~/.vim` or inside `~/.config/nvim`
- if neither, ask the user which implementation they want to use (both is
  an option as well)
- write that value to the package manifest (`vim-plugin.json`)
- ask the user if they want to use a lockfile
- also prompt the user to supply a directory in which to install the plugins:

```
# vim default:
~/.vim/vpm/

# nvim default:
~/.local/share/nvim/vpm/
```

this will be written to the `vim-plugin.json` file, along with the location of
the config file and so on.

There will be a small vimscript wrapper around the installed packages,
which will allow for the implementation of:

- rules about when to load certain plugins (lazy loading), e.g. for only
  certain filetypes, some plugins for neovim others just for vim, etc

these rules are set in `vim-plugin.json`, and the little vimscript wrapper
basically just reads the rules from here.

this will be copied automatically to the directories:

```
# vim
~/.vim/autoload/vpm.vim

# nvim
~/.local/share/nvim/site/autoload/vpm.vim
```

### installing and updating packages

```
vpm add ghuser/pluginname
```

adds a new plugin. prompts the user for any special rules it should
follow:

```
$ vpm add pangloss/vim-javascript
Adding pangloss/vim-javascript...
should this plugin only be loaded for certain filetypes? (e.g.
JavaScript, Elm, etc):
$ JavaScript
does this plugin require any manual setup (y/n)?
$ n
saving configuration to $path_to_vim-package.json...done

pangloss/vim-javascript: Cloning into '/path/to/vim-plugin/vim-javascript'
```

there should also be a `vpm edit author/package` command that goes through
these questions again.

this can also accept an `@` argument with a SHA1 or branch to use:

```
$ vpm add valloric/youcompleteme@878d329e8faa5a0ce3b357454f732f6b91272a3a
Adding pangloss/vim-javascript...
should this plugin only be loaded for certain filetypes? (e.g.
JavaScript, Elm, etc):
$ 
does this plugin require any manual setup?
$ python install.py --tern-completer --gocode-completer --clang-completer
saving configuration to $path_to_vim-package.json...done

valloric/youcompleteme: Cloning into '/path/to/vim-plugin/youcompleteme
running postinstall... # shows some sort of spinner, error output if there
is any, otherwise it finishes with like a check mark or something.
```

that particular commit will be checked out, instead of just pulling the
latest `master`. if a commit or a branch is specified that value will be
written to `vim-package.json` (even if the user doesn't use the lockfile).

the 'cloning' display will output the value of the git command (i.e. show
the progress).


```
$ vpm update
updating plugins # show spinner
'tpope/vim-commentary' Cloning into '/path/to/vim-plugin/vim-commentary'
'tpope/vim-rsi' Already up to date!
'tpope/vim-sensible' Receiving objects:  23% (1534/6460), 11.68 MiB | 23 KiB/s
'valloric/youcompleteme' Running postinstall...
'Chiel92/vim-autoformat' done # checkmark?
```

this will update all plugins to their latest versions. Can also update
only a specific plugin by passing the name as an argument to the command.
the `update` command will ignore plugins that have a commit specified in
`vim-plugin.json`, but will pull the latest `HEAD` for plugins that have
a branch specified. For a plugin with a commit SHA specified, calling

```
vpm update foo/bar
```

will prompt the user to confirm they want to upgrade, given that a SHA has
already been specified.

```
vim install
```

This will just install the packages saved in `vim-package.json`, checking
out the commits saved in the lockfile (if in use), and otherwise pulling
the latest `master` (provided there is not a commit / branch specified in
`vim-package.json`)

## lockfile and vim-package.json

If the user decides to use a lockfile when they are setting things up then
any `add` or `update` usage will write values to the lockfile hold the
commit SHA of `HEAD` at install-time for each package. This is just kind
of to make it easier to share vim configs that are known to work together,
and not have to worry about having different versions installed on
different machines, make it easier to diagnose and deal with problems that
come up with a particular plugin, etc.

Anyway, here is an example `vim-package.json` and an accompanying
lockfile, showing a basic configuration for `nvim`:

```json
{
  "vim_implementations": [
    {
      "nvim": {
        "plugin_dir": "~/.local/share/nvim/vpm"
      }
    }
  ],
  "use_lockfile": "true",
  "vpm_version": "0.0.1",
  "dependencies": {
    "pangloss/vim-javascript": {
      "for": [
        "JavaScript"
      ]
    },
    "valloric/youcompleteme": {
      "checkout": "878d329e8faa5a0ce3b357454f732f6b91272a3a",
      "postInstall": "python install.py --tern-completer --gocode-completer --clang-completer"
    },
    "rdnetto/YCM-Generator": {
      "checkout": "stable"
    },
    "tpope/vim-commentary": {}
  }
}
```

a `vim-package.lock` corresponding to this package manifest might look
like:

```
pangloss/vim-javascript:  6e8811c32483eec959e6f2981ad870656cd5b4ec
valloric/youcompleteme:   878d329e8faa5a0ce3b357454f732f6b91272a3a
rdnetto/ycm-generator:    4d92151f0b29afadacdd3a6c90e5a449afc34bb1
tpope/vim-commentary:     73e0d9a9d1f51b6cc9dc965f62669194ae851cb1
```
