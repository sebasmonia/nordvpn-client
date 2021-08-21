# Nordlocations

Small tool to lookup NordVPN servers and map them to a location. This could be a spreadsheet that I update every few months, but where's the fun in that!

Depends on [nodgui](https://www.autistici.org/interzona/nodgui.html), [dexador](https://github.com/fukamachi/dexador), [jonathan](https://github.com/Rudolph-Miller/jonathan), the NordVPN [servers API](https://api.nordvpn.com/server) , and ArcGIS [REST API](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-reverse-geocode.htm).


## Installation

You can compile your own version with the included `build.sh` script. You need SBCL with Quicklisp, and also Tcl and Tk.

Included in the "binary" directory is a pre-packaged Tcl+Tk downloaded from http://kitcreator.rkeene.org/kitcreator (the executable renamed from `tclkit` to `wish`) and `nordlocations` that should work on Linux x64. You can drop both files to `~/.local/bin`.

## Usage

Execute "nordlocations", it should display a list of servers. Clicking a server name will show its location info.



