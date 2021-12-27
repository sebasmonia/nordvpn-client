# NordVPN Client

Small UI tool to manage NordVPN connections. Written in Common Lisp, it uses `nmcli`, and the NorVPN API (as [described in](https://sleeplessbeastie.eu/2019/02/18/how-to-use-public-nordvpn-api/) these [two posts](https://sleeplessbeastie.eu/2019/01/14/how-to-use-terminal-to-display-servers-recommended-by-nordvpn/) and with some more details form [this tool](https://github.com/trishmapow/nordvpn-tools)) to either get a server by country/city or the default recommended for your current location.

I built this because I run Fedora Silverblue, so I prefer something I can run without layering into the base OS. And also to sharpen my Common Lisp :)

## Dependencies

* [nodgui](https://www.autistici.org/interzona/nodgui.html) to build the UI using Tk
* [dexador](https://github.com/fukamachi/dexador) for web requests
* [shasht](https://github.com/yitzchak/shasht) to parse JSON

## Installation [Outdated until I finish v2 :)]

You can compile your own version with the included `build.sh` script. You need SBCL with Quicklisp, and also Tcl and Tk.

Included in the "binary" directory is a pre-packaged Tcl+Tk downloaded from http://kitcreator.rkeene.org/kitcreator (the executable renamed from `tclkit` to `wish`) and `nordlocations` that should work on Linux x64. You can drop both files to `~/.local/bin` and then run `nordlocations`.

## Usage [Outdated until I finish v2 :)]

Execute `nordlocations`, then click the button or press [Space] to fetch the list of servers from NordVPN. Focus moves to the search box.

![filter290](/images/filter290.png)

Pressing [Enter] moves focus to the list to navigate with the keyboard, you can also use the mouse to click any server and see where it is located.

![showlocation](/images/showlocation.png)

