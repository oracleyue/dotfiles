-- This is v0.6 of the custom script for AlfredApp for iTerm 2.9+
-- Please see https://github.com/stuartcryan/custom-iterm-applescripts-for-alfred/
-- for the latest changes.

-- Please note, if you store the iTerm binary in any other location than the Applications Folder
-- please ensure you update the two locations below (in the format of : rather than / for folder dividers)
-- this gets around issues with AppleScript not handling things well if you have two iTerm binaries on your system... which can happen :D

on alfred_script(q)
	run script "
		on run {q}
			tell application \":Applications:iTerm.app\"
				activate
					create window with default profile
					select first window
				tell the first window
					tell current session to write text q
				end tell
			end tell
		end run
	" with parameters {q}
end alfred_script
