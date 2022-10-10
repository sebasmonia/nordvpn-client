# NordVPN Client

Small UI tool to manage NordVPN connections. Written in Common Lisp, it uses `nmcli`, and the NordVPN API (as [described in](https://sleeplessbeastie.eu/2019/02/18/how-to-use-public-nordvpn-api/) these [two posts](https://sleeplessbeastie.eu/2019/01/14/how-to-use-terminal-to-display-servers-recommended-by-nordvpn/) and with some more details form [this tool](https://github.com/trishmapow/nordvpn-tools)) to either get a server by country/city or the default recommended for your current location.

I built this because I run Fedora Silverblue, so I prefer something I can run without layering into the base OS. And also to sharpen my Common Lisp :)

## Table of contents

<!--ts-->

   * [Dependencies](#dependencies)
   * [Installation](#installation)
   * [First time setup](#first-time-setup)
   * [Using the application](#using-the-application)
   * [Future improvements](#future-improvements)

<!--te-->

## Dependencies

* [nodgui](https://www.autistici.org/interzona/nodgui.html) to build the UI using Tk
* [drakma](https://edicl.github.io/drakma/) for web requests
* [shasht](https://github.com/yitzchak/shasht) to parse JSON

## Installation

You can compile your own version with the included `build.sh` script. You need SBCL with Quicklisp, and also Tcl and Tk.  
&nbsp;  
Included in the "binary" directory is a pre-packaged Tcl+Tk downloaded from http://kitcreator.rkeene.org/kitcreator (the executable renamed from `tclkit` to `wish`) and `nordvpn-client` that should work on Linux x64, and definitely works if you use Fedora Silverblue.  
Drop both files to `~/.local/bin` and then run `nordvpn-client` from the terminal.

If you wish to autostart the application, you should copy `NordVPN Client.desktop` from the "launcher" directory to your `~/.config/autostart` directory, and then edit the line:
```
Exec=/home/{user goes here}/.local/bin/nordvpn-client
```
for your particular user.  

## First time setup

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

## Using the application

You are now ready to execute `nordvpn-client`. What to do next should be, hopefully, somewhat obvious. But just in case:

![window](/images/window.png)

When the application starts, the focus will be in the "Detect best local server" (1) button, press the spacebar or click it to display the information reported by the NordVPN API for "closest server with lowest load".  
Alternatively, you can use the listbox (2) to search for a country and city, and by clicking the item you would similarly get the information for the best available server. In the screenshot I clicked "Argentina  -  Buenos Aires" and
the "Recommended server" section has what the API returned as lowest load for that city.  
The "CONNECT" (3) button is grayed out until there's a server recommendation. Then it is enabled and when clicking it:  
1. The ovpn config file for the server is downloaded from Nord  
2. The connection is imported into Network Manager and the credentials are set using the information in the keyring  
3. Finally, we connect to the the newly created VPN  

The last two steps are done using `nmcli`. The connection is imported using `--temporary` so that it is discarded after each restart, although in my experience they do linger between reboots.  
Since almost every action in the UI is blocking, there's a Status at the bottom (4) that displays information on what's happening (getting data from the API, downloading the ovpn file, etc.)  

## Future improvements

I don't expect to make any changes to this application, but if there were more users, some good ideas:  

* Make the keys used to retrieve user/pass configurable
* Support connecting to the other servers for each location, not only the "best" one
* Pool `nmcli` (or receive notifications via D-Bus I guess?) to show real-time "Status" after connecting
