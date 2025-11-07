This replication 

## Software

I used [Stata](http://www.stata.com)'s StataNow/MP 19.5 on macOS Sequoia (version 15.6.1). I had to install [Homebrew](https://brew.sh) and [Xcode](https://developer.apple.com/xcode/), which is available on the AppStore. All of my code was written on [VS Code](https://code.visualstudio.com/) or on the [Stata](http://www.stata.com) do-file editor.

## MacOS Replication Instructions

-Open Unix/Linux/MacOSX command line. I had zsh - Version 2.14 (455) at the time of this project. To do this, you can press Command + Space and write terminal.

- Run the following, if you have not already
```bash
brew install wget
```
-We must also change Stata path.
 ```bash
nano ~/.zshrc
```
-Now run 
  ```bash
export PATH="/Applications/StataNow/StataMP.app/Contents/MacOS:$PATH"
```
-Save with Control+X and Enter to save
  ```bash
source ~/.zshrc
```
-Then, simply type
```bash
make
```
-The Makefile will run all of the code needed to replicate my results from scratch. You have to set the folder where the main Makefile is using cd and typing the folder path.