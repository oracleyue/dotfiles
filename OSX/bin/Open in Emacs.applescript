-- To use this script to allow opening files in Emacs via double-clicking, open Automator.app -> choose new app -> add "run AppleScript" -> copy this script into it -> save as an app.

on run {input, parameters}
	
	set posixPath to POSIX path of input
	
	tell application "iTerm" to do shell script "/usr/local/bin/emacsclient -nc --socket-name=main " & quoted form of posixPath
	
	return input
	
end run
