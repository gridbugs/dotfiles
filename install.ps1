Set-StrictMode -Version Latest

function ReplaceDir {
    param (
        [string]$Path,
	[string]$Destination
    )

    if (Test-Path -Path $Destination) {
        Remove-Item -Path $Destination -Recurse -Force
    }
    Copy-Item -Path $Path -Destination $Destination -Recurse
}

ReplaceDir -Path "$PSScriptRoot\nvim" -Destination "$HOME\AppData\Local\nvim"
