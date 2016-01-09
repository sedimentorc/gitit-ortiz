# Gitit Bigger Ortiz

I have modified the gitit bigger repository to support plugins.

I need to incorporate these changes into the Dockerfile.

# Installation Log

## Install gitit 

	sudo apt-get install -y git gitit supervisor libghc-zlib-dev libghc-filestore-data cabal-install graphviz ditaa
	# fonts cannot be found
	#sudo apt-get install fontconfig-infinality-ultimate freetype2-infinality-ultimate cairo-infinality-ultimate ibfonts-meta-base otf-tex-gyre-math-ibx
	 
	sudo apt-get install texlive texlive-latex-extra inkscape dvipng pdf2svg pstoedit s5
	cabal update
	# do the following one at a time
	cabal install split,parsec, pandoc, gitit, pandoc-citeproc
	# optional, but useful, (~1GB usage)
	sudo apt-get install haskell-platform haskell-platform-doc haskell-platform-prof python-pip

Make sure to add /home/user/.cabal/bin to path!

##Reinstall gitit
Adding gitit from the repo allows compiling against latest pandoc, which is necessary to allow some plugins to function

	sudo apt-get install stack
	#updates ghc
	stack setup
	# add PATH = "/home/orc/.stack/programs/x86_64-linux/ghc-7.10.2/bin:$PATH" to ~/.profile (apparently it's better to add here than .bash_aliases, see http://askubuntu.com/questions/234623/how-can-i-add-a-directory-to-my-path)
	cd ~
	git clone https://github.com/jgm/gitit
	cd gitit
	stack install	


Convert .page to .md, to use standard markdown file format

	cd wikidata
	rename 's/.page$/.md/' *.page 
	git add .	
	git commit -m "rename extensions"

# Gitit Bigger (Translated to English)
Gitit Bigger: Git and Markdown based wiki, Bootstrap, ace Editor, syntax highlight and docker deploy support.

Based on Git and Markdown's Awesome Wiki system, Bootstrap, Ace editor enhancements, and support Docker deployment.

# Start Start Bigger
> Assuming you have installed Git and Gitit

- regular version

`` `Bash
git clone https://github.com/shajiquan/gitit-bigger ~ / workspace / gitit;
cd ~ / workspace / gitit;
cp sample.gitit.conf my-gitit.conf;
./run/run.sh start;
`` `

- Docker Edition

`` `Bash
docker run -d --name gitit -p 7500: 7500 shajiquan / gitit;
`` `

Access: [http://127.0.0.1:7500] (http://127.0.0.1:7500)

# Demo & Screeenshots
## Demos
- Gitit Bigger: [http://wiki.shajiquan.com/gitit-bigger] (http://wiki.shajiquan.com/gitit-bigger) (Read Only)
- Gitit official: [http://gitit.net] (http://gitit.net)

## Screenshot Screenshots
### View View
- [View.png] (https://github.com/shajiquan/gitit-bigger/blob/master/screenshots/view.png)
- [View-ipad-mini.png] (https://github.com/shajiquan/gitit-bigger/blob/master/screenshots/view-ipad-mini.png)
- [View-iphone6.png] (https://github.com/shajiquan/gitit-bigger/blob/master/screenshots/view-iphone6.png)

### Edit Edit
- [Edit.png] (https://github.com/shajiquan/gitit-bigger/blob/master/screenshots/edit.png)
- [Edit-ipad-mini.png] (https://github.com/shajiquan/gitit-bigger/blob/master/screenshots/edit-ipad-mini.png)
- [Edit-iphone6.png] (https://github.com/shajiquan/gitit-bigger/blob/master/screenshots/edit-iphone6.png)

# Gitit Key Features
- No Database
- Git version control
- Markdown format
- Wiki-based:
  - Subdirectories, unlimited directory (this is the reason I abandoned some other wiki-like system)
  - Chinese directory, Chinese title, Chinese classification
  - Perfect support Chinese search
  - Support Categories
  - Support for custom title

- Code highlighting
- Support formulas, etc. (I basically do not)
- Export epub, etc. (based on pandoc)

More about Gitit installation, deployment, optimization of Chinese introduction and description, see:
- [Gitit_base.md introduction, installation, deployment] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_base.md)
- [Gitit_config.md configuration] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_config.md)

# Gitit Bigger VS Gitit
- Bootstrap Template
- Ace Editor
- Ace Editor Edit mode is set to `markdown`.
  - Support for code highlighting
  - Search, Replace (cmd + option + f)
  - Tab to indent, recovery (tab, shift + tab)
  - hot key
  - ...

- Way to start using the configuration file, multiple instances easily share resources
- Support for custom editors Ace enable / disable, pattern, style, and other configurations
- The support Markdown mode shortcuts
- Mathematical formulas: Support MathJax enable / disable, custom MathJax source
- Code highlighting: supports highlight.js Highlighting Code - Gitit highlighted the need server support
- Increase start, part, automatic backup script or help

# Note: wikidata warehouse
Remember to check your `wikidata /` git directory configuration. Please note:
- `Wikidata` folder is a local warehouse, all of wiki pages are saved to here;
- When `Gitit` starts, it checks this folder exists, if does not exist, Gitit creates it and initializes it to a local warehouse
- Only submit documents to the repository, it will be added to the wiki Gitit.
- If you want this repository to your remote repository binding synchronization: you need:
  - Clone your repository to `wikidata` wiki folder:` git clone your-wikidata.git / wikidata`, or:
  - Run the `git remote` related commands, so` wikidata` folder and connect your remote repository;

`` `Bash
# If the folder does not already exist wikidata
cd ~ / workspace / gitit
git clone your-wikidata.git ./wikidata
git branch --set-upstream-to = origin / master master
# Start gitit service: ./run/run.sh start

# If wikidata already exists, but does not bind your remote repository
cd wikidata
git remote add origin path / to / your-wikidata.git
git branch --set-upstream-to = origin / master master
# Start gitit service: ./run/run.sh start
`` `

# Configuration, custom JS / CSS
Gitit Bigger provide a degree of configuration.

You can pass the `templates / page_more_scripts.st` defined` BIGGER_SETTINGS_APPEND` object to override the default configuration.

include:
- Ace editor
- Markdown
- MathJax mathematical formula widget
- Highlightjs code highlighting
- Google Analytics statistics

For details, see: [gitit_bigger_config.md] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_bigger_config.md)

# Tools Utils
- Start control
- Batch modify extension
- Automatic backup aid

For details, see: - [gitit_bigger_utils.md] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_bigger_utils.md)

# By Shell editing wiki
Not through the web interface, you can operate our Gitit Wiki. background:
1. `wikidata`: wiki pages warehouse
2. Only files submitted to the warehouse, will be added to the wiki Gitit.

`` `Bash

# Enter wikidata directory
cd / path / to / your / wikidata

# New file and enter some string
touch new_page.md
echo "hello shajiquan" >> new_page.md

# Add new files to the repository
git add new_pge.md
git commit -m "add new_page.md file"

# Update the file and submitted to the warehouse
echo "new line" >> old_page.md
git add old_page.md
git commit -m "update old_page.md"

# Push to a remote repository
git push origin master
`` `

# Multiple instances
By way of the configuration file, you can easily run multiple Gitit instances. In addition wiki.shajiquan.com outside, I also run another instance to do private notes.
1. `cp sample.gitit.conf my-gitit-private.conf;`
2. Update the `my-gitit-private.conf` in the configuration, in particular: the port, wikidata directory (see [gitit_config.md configuration] (https://github.com/shajiquan/gitit-bigger/blob/ master / docs / gitit_config.md))
3. Start a new instance: `gitit -f my-gitit-private.conf`

note
- If the `my-gitit.conf` and` my-gitit-private.conf` both instances use a different user, cookies may cause confusion.
- When the service ends, consider using `nginx` do reverse proxy, bind a secondary domain name for each instance.
- Locally, you can use `localhost` and to deal with the issue` cookie domain 127.0.0.1` a class.

# Docs
- [Gitit introduction, installation, deployment] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_base.md)
- [Gitit configuration item description] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_config.md)
- [Gitit-Bigger-Docker version introduced] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_bigger_docker.md)
- [Gitit-Bigger Custom Note] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_bigger_config.md)
- [Gitit-Bigger Tools .md] (https://github.com/shajiquan/gitit-bigger/blob/master/docs/gitit_bigger_utils.md)

# Feedback, suggestions, contact
- Github Issues: [https://github.com/shajiquan/gitit-bigger/issues] (https://github.com/shajiquan/gitit-bigger/issues)
- Email: shajiquan@gmail.com
- QQ: 2848559858

# ChangeLog
## V0.1.3
- Adjust page width, more reasonably accommodate large and small screen

## V0.1.2
- To the content area pictures made responsive handling, no longer stays rotten page

## V0.1.1
- Rewrite Document
- Increase the number of small configuration items

## V0.1beta
- New configuration;
- Repair MathJax JS files into error;

## V0.0.0.1
- Initial implementation, integration ace, markdown, highlightjs etc.

# Links
- Github: [https://github.com/shajiquan/gitit-bigger] (https://github.com/shajiquan/gitit-bigger)
- DockreHub: [https://hub.docker.com/r/shajiquan/gitit] (https://hub.docker.com/r/shajiquan/gitit)
- Demo: [http://wiki.shajiquan.com/gitit-bigger](http://wiki.shajiquan.com/gitit-bigger) (Read Only)
- Gitit official site: [http://gitit.net] (http://gitit.net)
- Gitit official github: [https://github.com/jgm/gitit](https://github.com/jgm/gitit)
- Deployment gitit Wiki on Archlinux: [http://www.360doc.com/content/12/0518/21/21412_211977928.shtml](http://www.360doc.com/content/12/0518/21/ 21412_211977928.shtml)
- Gitit - git based wiki: [http://walkingice.blogspot.hk/2011/11/gitit-git-based-wiki.html](http://walkingice.blogspot.hk/2011/11/gitit-git -based-wiki.html)
- Hyzual / docker-gitit: [https://github.com/Hyzual/docker-gitit](https://github.com/Hyzual/docker-gitit)
- Gitit Bootstrap template: [Changaco / gitit-bootstrap] (https://github.com/Changaco/gitit-bootstrap)
- Gitit ace editor support [Getting the Ace editor to work with gitit] (https://gist.github.com/lmullen/e2d2d4aabf84220c517a)

[! [Bitdeli Badge] (https://d2weczhvl823v0.cloudfront.net/shajiquan/gitit-bigger/trend.png)] (https://bitdeli.com/free "Bitdeli Badge")