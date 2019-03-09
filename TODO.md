Document tickets fixed
Document kernel changes vista
Document FILE_FLAG_OVERLAPPED choice
Document Console API/cooked etc
Document SetFilePointerEx
Document MSYS2 I/O redirection
Document OVERLAPPED 
Document Async requests out of order, prevent expensive seeks
Document ReadFile/WriteFile return status
Document neither buffer or overlapped allowed to be moved
Document Completion ports, (we use it over other methods)
Document COND_VAR and Slim reader write locks
Document CreateIoCompletionPort


* Consider extending the OVERLAPPED struct to contain pointers to the completions and events
* Rename or reconsider use of word alertable to describe when requests are services to avoid conflicts with alertable I/O
* Use multiple threads to handle GetQueuedCompletionStatus calls.
* cleanup completionCB

* vscode 
https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server
https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell
https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode
https://marketplace.visualstudio.com/items?itemName=jcanero.hoogle-vscode
https://marketplace.visualstudio.com/items?itemName=aaron-bond.better-comments
https://marketplace.visualstudio.com/items?itemName=alefragnani.Bookmarks
https://marketplace.visualstudio.com/items?itemName=CoenraadS.bracket-pair-colorizer
https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens
https://marketplace.visualstudio.com/items?itemName=spywhere.guides
https://marketplace.visualstudio.com/items?itemName=emmanuelbeziat.vscode-great-icons
https://marketplace.visualstudio.com/items?itemName=CoenraadS.bracket-pair-colorizer-2
https://marketplace.visualstudio.com/items?itemName=byi8220.indented-block-highlighting
https://marketplace.visualstudio.com/items?itemName=oderwat.indent-rainbow
https://code.visualstudio.com/docs/getstarted/settings

https://github.com/alanz/vscode-hie-server
https://github.com/haskell/haskell-ide-engine
https://code.visualstudio.com/docs/editor/command-line
https://hackage.haskell.org/package/phoityne-vscode
https://hackage.haskell.org/package/haskell-dap
https://github.com/phoityne/phoityne-vscode

* cabal bug empty include dir
* bug .exe suffix

