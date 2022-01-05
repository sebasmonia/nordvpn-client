# NordVPN Client

Small UI tool to manage NordVPN connections. Written in Common Lisp, it uses `nmcli`, and the NorVPN API (as [described in](https://sleeplessbeastie.eu/2019/02/18/how-to-use-public-nordvpn-api/) these [two posts](https://sleeplessbeastie.eu/2019/01/14/how-to-use-terminal-to-display-servers-recommended-by-nordvpn/) and with some more details form [this tool](https://github.com/trishmapow/nordvpn-tools)) to either get a server by country/city or the default recommended for your current location.

I built this because I run Fedora Silverblue, so I prefer something I can run without layering into the base OS. And also to sharpen my Common Lisp :)

## Dependencies

* [nodgui](https://www.autistici.org/interzona/nodgui.html) to build the UI using Tk
* [dexador](https://github.com/fukamachi/dexador) for web requests
* [shasht](https://github.com/yitzchak/shasht) to parse JSON

## Installation

You can compile your own version with the included `build.sh` script. You need SBCL with Quicklisp, and also Tcl and Tk.

Included in the "binary" directory is a pre-packaged Tcl+Tk downloaded from http://kitcreator.rkeene.org/kitcreator (the executable renamed from `tclkit` to `wish`) and `nordvpn-client` that should work on Linux x64. You can drop both files to `~/.local/bin` and then run `nordvpn-client`.

## Usage

To be able to use the client, you need to add your NordVPN username and password to the GNOME Keyring, which is a one time only setup. The values should be stored under `nordvpn-client username` and `nordvpn-client password`.  
One way to do it, from the terminal:

```
$ secret-tool store --label='NordVPN Username' nordvpn-client username
Password: [type username here]
$ secret-tool store --label='NordVPN Password' nordvpn-client password
Password: [type password here]
```
If you want to verify that the values are correct, you can do this:
```
$ secret-tool lookup nordvpn-client username 
```
or lookup `password`, but be aware that the values are echoed back in plain text.  

After that one-time setup, execute `nordvpn-client`. Things should hopefully be somewhat obvious, but just in case:

![window](/images/window.png)

When the application starts, the focus will be in the "Detect best local server" (1) button, press the spacebar or click it to display the information reported by the NordVPN API for "closest server with lowest load".  
Alternatively, you can use the listbox (2) to search for a country and city, and by clicking the item you would similarly get the information for the best available server. In the screenshot I clicked "Argentina  -  Buenos Aires" and
the "Recommended server" section has what the API returned as lowest load for that city.  
The "CONNECT" (3) button is grayed out until there's a server recommendation. Then it is enabled and when clicking it:  
1. The ovpn config file for the server is downloaded from Nord  
2. The connection is imported into Network Manager (using `--temporary`) and the credentials are set using the information in the keyring  
3. Finally, the newly created VPN is opened  

The last two steps are done using `nmcli`.  
Since almost every action in the UI is blocking, there's a Status at the bottom (4) that displays information on what's happening (getting data from the API, downloading the ovpn file, etc.)  

## Possible improvements

I don't expect to make any changes to this application, but if there were more users, some good ideas:  

* Make the keys used to retrieve user/pass configurable
* Support connecting to the other servers, not only the "best" one
* Pool `nmcli` (or via D-Bus I guess?) to keep the "Status" after connecting updated. Right now after the little lock icon is in the notification area, this tool is useless :)
