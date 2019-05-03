# p

A simple password management tool. Passwords are stored as plain text files in a directory of your choosing. Your password directory is then easily managed using `p`.

## Install
Clone the repository and run `stack install`. Alternatively, a macOS build is available in `dist/`.

## Basic usage

First of all, create a directory where you want your passwords to be stored and then create a file called `.pconfig.json` in your home directory with the following contents:

```json
{
    "passwordsPath": "[your directory path here]"
}
```

To see a list of what you can do, just type `p`:

```
$ p
p is a simple password management tool

Usage: p COMMAND
  p

Available options:
  -h,--help                Show this help text

Available commands:
  gen                      Generates a new password
  cat                      Prints a password
  copy                     Copies a password to the clipboard
  ls                       Lists all password domains
  rm                       Removes a password domain
```

Type `p gen` to generate a password and copy it to the clipboard, without saving the password anywhere.

```
$ p gen
Generated password: C-yMSH5i!0K1iL,P?wB8
Password was copied to clipboard
```

To save the password, give it a name:

```
$ p gen gmail
Password written to /Users/JohnDoe/passwords/gmail
Generated password: $!;J;^npqA(_6RjY2z>V
Password was copied to clipboard
```

Some commands have extra options. Type `--help` after a command to learn more about it.

## License
MIT

