tell application "Finder"
	try
		set targetFolder to (folder of front window as alias)
	on error
		set targetFolder to (path to desktop folder)
	end try
	
	set targetPath to quoted form of (the POSIX path of targetFolder)
	set cdToPath to "cd " & targetPath	
end tell

tell application "iTerm"
	activate
	create window with default profile
	
	tell current session of current window
		write text cdToPath
	end tell
end tell
