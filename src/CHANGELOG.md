# Revision history for src

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## Description

Basic datatypes for writing code that handles poker (simulation, a poker website, solver etc.). The datatypes and their functions are well-tested. The goal is to eventually reach 100% code coverage and for this package to be kept to an industrial quality.

## HLS integration

Unfortunately there is currently a bug in HLS (or maybe hie-bios) such that when coding in the test directory, changes to the main library will not be seen from the test package until the HLS server is reloaded. In VS code, I have a keybinding in my global keybindings.json:

```
  {
    "key": "alt+f",
    "command": "workbench.action.tasks.runTask",
    "args": "rebuild and reload LSP"
  },
```

which links to the following task in this project's local `.vscode/tasks.json`:

```
    {
      "group": "build",
      "type": "shell",
      "label": "haskell clean & build",
      "command": "stack clean && stack build"
    },
    {
      "type": "shell",
      "label": "rebuild and reload LSP",
      "command": "${command:haskell.commands.restartServer}",
      // "dependsOn": [
      //   "haskell clean & build"
      // ]
    }
```

Note that you can get away without having to build/clean first. It seems to depend. But if you require cleaning first, you can simply uncomment the dependsOn field in the `rebuild and reload LSP` task.

## Tests

Tests are run using

```
stack test
```

Tests are written using the awesome [tasty-discover](https://hackage.haskell.org/package/tasty-discover) package which allows you to write mix tests with the QuickCheck, hspec, HUnit etc. packages.
