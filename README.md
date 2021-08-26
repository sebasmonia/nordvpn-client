# Nordlocations

Small tool to lookup NordVPN servers and map them to a location. This could be a spreadsheet that I update every few months, but where's the fun in that!

Depends on [nodgui](https://www.autistici.org/interzona/nodgui.html), [dexador](https://github.com/fukamachi/dexador), [jonathan](https://github.com/Rudolph-Miller/jonathan), the NordVPN [servers API](https://api.nordvpn.com/server) , and ArcGIS [REST API](https://developers.arcgis.com/rest/geocode/api-reference/geocoding-reverse-geocode.htm).


## Installation

You can compile your own version with the included `build.sh` script. You need SBCL with Quicklisp, and also Tcl and Tk.

Included in the "binary" directory is a pre-packaged Tcl+Tk downloaded from http://kitcreator.rkeene.org/kitcreator (the executable renamed from `tclkit` to `wish`) and `nordlocations` that should work on Linux x64. You can drop both files to `~/.local/bin` and then run `nordlocations`.

## Usage

Execute `nordlocations`, then click the button or press [Space] to fetch the list of servers from NordVPN. Focus moves to the search box.

![filter290](/images/filter290.png)

Pressing [Enter] moves focus to the list to navigate with the keyboard, you can also use the mouse to click any server and see where it is located.

![showlocation](/images/showlocation.png)

## Rationale

As a Fedora Silverblue user, I cannot install the RPM package that Nord provides. I can use their OpenVPN files, but the format Country + Server Number is not detailed enough.  
And, an excuse to sharper my Common Lisp :)






