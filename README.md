# Intro

**myo** is a [Neovim] plugin written in [Haskell] and powered by [ribosome] and [nvim-hs].

It provides several interacting features that include:
* [**UI**](#ui): automatic opening, closing and layouting of tmux panes
* [**Commands**](#commands): Execution of configurable shell commands in [tmux] or subprocesses
* [**Output**](#output): Parsing, rendering and navigation of messages produced by commands

**Note**: _highly experimental state_

**myo** benefits a lot from having per-project and per-project-type configuration for setting up language-specific commands.
If you don't have a solution for this, the author also maintains [proteome], a project management plugin.

![Haskell output screenshot](img/haskell-output.png)

# Install

There are two alternative methods for integrating this plugin into your Neovim.
In both cases, the plugin will bootstrap your machine with [stack] and install itself on startup, as well as rebuild
when the repository is updated.

## nvim-hs

The basic variant is to use the built-in package management facility of the rplugin provider.
In this case, you have to include [nvim-hs.vim]:

```vim
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'tek/myo-hs'
```

## chromatin

The author maintains a manager for [ribosome]-based plugins called [chromatin] that has additional features.
You will be presented with a terminal buffer containing the output of the installation routines whenever something is
built.

```vim
Plug 'tek/chromatin-hs'
Plug 'tek/myo-hs'
```

# Configuration

There are a few settings that, when changing their variables, will trigger reconfiguration of the corresponding
component:

* `myo_ui`
* `myo_commands`

**myo** uses several autocommands to detect changes.

For the examples in this Readme, I will be using a setup for compiling and testing [Scala] with [sbt].

## MyoDiag

This command will display some information about the plugin in a scratch buffer.

# UI

The purpose of the UI component is to allow creating tracked tmux panes with a fixed layout from mappings that are
cleaned up when vim quits.
The UI is configured through the variable `myo_ui` whenever it is modified.
Its schema looks like this:

```vim
let s:sbt_pane =
      \ {
      \ 'layout': 'make',
      \ 'ident': 'sbt',
      \ 'minSize': 0.5,
      \ 'maxSize': 35,
      \ 'position': 8
      \ }
let g:myo_ui = {
      \ 'panes': [s:sbt_pane]
      \ }
```

These settings will be combined with the built-in default layout that consists of determining the window and pane in
which Neovim has been started and a vertical layout named `make` positioned to the right of the Neovim pane that
contains one pane, also called `make`, which is used as the default target when executing system commands.

It is possible to set up much more complex layouts, with windows and sessions yet to come.
The `myo_ui` variable may contain a key `layouts` with the same schema as for panes.

The library used for interacting with tmux is [chiasma].

### Pane Fields

* `layout` is the name of the parent layout of the pane.
* `pin` causes this pane to be opened whenever its layout becomes visible, i.e. when another pane in it is opened.
   The default `make` pane has this set.
* `position` is a value that orders panes in a layout. It can have any numerical value.
* `fixed_size` tries to make a pane always have this number of lines or columns.
* `minimized_size`, default `2`, is the number of cells that the pane gets when minimized.
* `weight` is used to distribute surplus space onto panes.

Panes open automatically when a command is executed in them, but there are functions for doing so manually.

## MyoToggle

```vim
call MyoToggle('sbt')
```

will open the pane `sbt` in the lower right corner, or, if it is open already, minimize it to a size of two cells in the
direction of its parent layout (in this case, vertically).

# Commands

A command consists of a list of shell commands and some optional values:
* a tmux pane in which to execute it
* a shell command in which to execute it
* a runner (tmux or subprocess)
* a language for parsing its output

Commands are read from the `myo_commands` variable:

```vim
let s:sbt_cmd = {
      \ 'ident': 'sbt',
      \ 'lines': ['sbt'],
      \ 'target': 'sbt',
      \ 'lang': 'scala',
      \ }
let s:compile_cmd = {
      \ 'ident': 'compile',
      \ 'lines': ['test:compile'],
      \ 'target': 'sbt',
      \ 'lang': 'scala',
      \ }
let g:myo_commands = { 'system': [s:sbt_cmd], 'shell': [s:compile_cmd] }
```

This snippet defines a _system command_ (run directly in the tmux pane) with the command line `sbt` that will be executed in the previously created pane `sbt` (defined by the `target` field).

Next, it defines a _shell command_ named `compile`, which has as its target the name of a system command, here `sbt`.
This means that when running this command, it will be sent to the tmux pane running `sbt` (it will be started if
necessary) and some interactions with it, like output parsing, will be deferred to the system command.
Its command line is `test:compile`, which is the text sent to `sbt`.

### Command Fields

* `ident` is the name of the command, to be used with `MyoRun` and `MyoParse`.
* `lines` is a list of command lines that will be executed in tmux. For subprocess commands, only one line is valid.
* `target` points to a pane or shell in which the command will be run.
* `lang` is used to find the appropriate parser for command output. Currently supported: `scala`, `haskell`.
* `runner` may be `tmux` or `proc` (simple subprocess).

## MyoRun

This Neovim command/function triggers the execution of a **myo** command:

```vim
MyoRun compile
```

This invocation will:
* create the tmux panes for `sbt` (which will also create `make` since it is pinned).
* run `sbt` in its pane
* send `test:compile` to the `sbt` pane

Subsequent invocations will check the stored process ID and skip the second step if it is alive.

## MyoReRun

This function allows you to run commands from the history.
For example, to run the previous command again:

```vim
call MyoReRun(0)
```

## MyoVimTest

**myo** features support for [vim-test], which is a plugin with many built-in heuristics for determining the appropriate
command to execute something relating to the current cursor position.
When running this function:

```vim
call MyoVimTest()
```

**myo** will call [vim-test] and execute its output in the pane or shell configured by `g:myo_test_pane` or
`g:myo_test_shell` (default being `make`).

# Output

When a command is executed, its output is read into **myo**'s state.
Running the command

```vim
MyoParse
```

will run the appropriate parser, read from the command option `pane` or the variable `g:myo_test_lang` (currently only
`haskell or ``scala`), and display its result in a scratch window, as showcased in the screenshot at the top of the
Readme.

After parsing, the cursor will be set to the last of first error (depending on the values of `g:myo_output_jump_first`
and `g:myo_output_auto_jump`).
Using the functions `MyoPrev` and `MyoNext`, you can cycle through the error list.

[Neovim]: https://github.com/neovim/neovim
[Haskell]: https://www.haskell.org
[ribosome]: https://github.com/tek/ribosome-hs
[chromatin]: https://github.com/tek/chromatin-hs
[nvim-hs]: https://github.com/neovimhaskell/nvim-hs
[tmux]: https://github.com/tmux/tmux
[proteome]: https://github.com/tek/proteome-hs
[nvim-hs.vim]: https://github.com/neovimhaskell/nvim-hs.vim
[Scala]: https://scala-lang.org
[sbt]: https://scala-sbt.org
[chiasma]: https://github.com/tek/chiasma-hs
[vim-test]: https://github.com/janko/vim-test
