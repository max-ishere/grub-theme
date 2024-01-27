# GRUB theming utilities
*Work in progress*

The purpose of this project is to make the process of making GRUB themes much faster and easier for the average user.
Normally the process would look like this:

1. Create a VM with GRUB as the bootloader
2. Start writing the theme
3. Recompile GRUB config
4. Reboot
5. Realize something is off by a pixel / there's more work to do
6. Repeat from step 2 untill:
  - Theme is finished
  - Theme is good enough and you're tired
  - You burn out

Paired with the fact that the iteration cycle is quite slow, this process is going to drive you nuts after a while. Oh,
and did you know that the documentation is quite lacking too? Like an entire optin was missing and some details are
ommited? Yeah this is very fun. /s

It is time to put an end to this suffering. I decided to make a tool for previewing the GRUB theme. While it is possible
that the preview isn't going to be pixel perfect, I hope this tool can get you at least 90% there and you can use a VM
or real hardware to polish whatever doesn't quite line up.

**TL;DR:** this is a GRUB theme preview tool.

# Roadmap

## MVP

- [ ] A CLI that generates a preview image given a GRUB theme directory and GRUB boot menu state (progress bar state,
  menu items, etc)

## Nice to have

- [ ] An interactive editor / automatic reload (pref. using WASM so it can be hosted on GH pages and a local version
  too)
- [ ] A Github Action to generate previews that can be used to provide release screenshots, update READMEs, etc.
