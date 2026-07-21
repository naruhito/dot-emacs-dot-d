# 事前準備
# echo $PROFILE
# Get-ExecutionPolicy -List
# Set-ExecutionPolicy RemoteSigned -Scope CurrentUser

Set-PSReadLineOption -EditMode Emacs
function nt { wt -w 0 nt -d $PWD }
