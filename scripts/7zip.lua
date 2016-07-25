-- the path of 7z.exe or 7zG.exe
_7z = '"C:\\Program Files\\7-Zip\\7z.exe"';

function Compress7z(_output, _filenames, _password, _close)
  _filenames = Table.Concat(_filenames, " ", 1, Table.Count(_filenames))
  if String.Compare(_password, "") ~= 0 then
    _password = "-p" .. _password .. " "
  else
    _password = ""
  end
  if _close then
    return File.Run(_7z, 'a '.. _output .. ' ' .. _password .. _filenames, "", 1, true)
  else
    return File.Run("cmd.exe", '/K ' .. _7z .. ' a '.. _output .. ' ' .. _password .. _filenames, "", 1, true)
  end
end

-- the output 7z file to create, double quoted
output = '"C:\\Temp\\Wallpapers.7z"'
-- a table with all filenames, double quoted
filenames = {'"C:\\Windows\\Web\\Wallpaper\\*"'}
-- the password to protect the file without spaces, to don't use password use an empty string ("")
password = "password"
-- if close or not the command window
closewindow = true

-- compress the file with 7zip
Compress7z(output, filenames, password, closewindow)

-- open the folder where is the output and selects the new file created
File.Run("explorer.exe", "/select," .. output, "", 1, false)
