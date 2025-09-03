{
  gum,
  wl-clipboard,
  screenshotImpl,
}:
{
  name = "menu";
  runtimeInputs = [
    gum
    wl-clipboard
  ];
  text = ''
    power_menu () {
        opt=$(gum choose --no-show-help Lock Sleep Suspend Hibernate Shutdown --header="Power:")
        case $opt in
      Lock)
          swaylock;;
    	Sleep)
    	    systemctl sleep;;
    	Suspend)
    	    systemctl suspend;;
    	Hibernate)
    	    systemctl hibernate;;
    	Shutdown)
    	    gum confirm --no-show-help "Shutdown?" && shutdown now;;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    link_menu () {
        opt=$(gum choose --no-show-help LinkedIn Personal GitHub --header="Link:")
        case $opt in
    	LinkedIn)
    	    wl-copy "https://www.linkedin.com/in/jonathanmfung";;
    	Personal)
    	    wl-copy "https://jonathanmfung.com";;
      GitHub)
          wl-copy "https://github.com/jonathanmfung";;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    main_menu (){
        opt=$(gum choose --no-show-help Power Link Screenshot --header="Menu:")
        case $opt in
    	Link)
    	    link_menu;;
    	Power)
    	    power_menu;;
    	Screenshot)
    	    ${screenshotImpl}/bin/screenshot;;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    main_menu
  '';
}
