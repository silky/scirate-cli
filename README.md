# scirate-cli

Terminal-based browsing tool for <https://scirate.com>. Trade scrolling for
pressing buttons!


![](./images/scirate-cli.png)


# Usage

- Clone the repo:

```
git clone https://github.com/silky/scirate-cli.git
```

- Install this via stack: 

```
stack install
```

- Get your scirate cookie, when you are logged in, by looking in the
   the devtools of google chrome. You want the one that starts with:
   `_scirate3_session`.

- Make a folder `~/.scirate-cli` and make a file `~/.scirate-cli/.env` and
   put the following:

  ```
  SCIRATE_COOKIE=<that cookie value>
  ```

- Run `scirate-cli --new --range 1` to query todays papers!


Once you complete a session (i.e. all papers decided on) it will commit your
scites.

You have to open the `Open later` papers manually by running:

```
sh ~/.scirate-cli/openLater.sh
```

You can resume a session by just running `scirate-cli`.

# Notes

This tool does no syncing really. It only gets the paper-information from
scirate, and then when you exit, prompts to push back all the scites you made.
If you resume again, it will prompt again to push the scites, _even though_
they were already done. So just hit "n" the second time. :shrug:

Feel free to help out with a PR if you see something you want to fix or would
like to add :) In particular, the colour scheme I've picked may not suit your
terminal colour definitions.

#### Trivia

Alternative name for this project:

- scimate
