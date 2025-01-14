{
  gum,
  wl-clipboard,
}:
{
  name = "menu";
  runtimeInputs = [
    gum
    wl-clipboard
  ];
  text = ''
    power_menu () {
        opt=$(gum choose --no-show-help Sleep Suspend Hibernate Shutdown --header="Power:")
        case $opt in
    	Sleep)
    	    systemctl sleep;;
    	Suspend)
    	    systemctl suspend;;
    	Hibernate)
    	    systemctl hibernate;;
    	Shutdown)
    	    gum confirm --no-show-help "Shutdown?" && systemctl shutdown;;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    link_menu () {
        opt=$(gum choose --no-show-help LinkedIn Personal --header="Link:")
        case $opt in
    	LinkedIn)
    	    wl-copy "https://www.linkedin.com/in/jonathanmfung";;
    	Personal)
    	    wl-copy "https://jonathanmfung.github.io";;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    main_menu (){
        opt=$(gum choose --no-show-help Power Link --header="Menu:")
        case $opt in
    	Link)
    	    link_menu;;
    	Power)
    	    power_menu;;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    main_menu
  '';
}
