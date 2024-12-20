{
  fuzzel,
  wtype,
}:
{
  name = "menu";
  runtimeInputs = [
    fuzzel
    wtype
  ];
  text = ''
    confirm () {
        opt=$(echo -e "No\nYes" | fuzzel -d --prompt="Confirm $1: " -l 2)
        case $opt in
    	No)
    	    echo "Cancelling $1";;
    	Yes)
    	    $1;;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    power_menu () {
        opt=$(echo -e "Sleep\nSuspend\nHibernate\nShutdown" | fuzzel -d --prompt="Power: " -l 4)
        case $opt in
    	Sleep)
    	    systemctl sleep;;
    	Suspend)
    	    systemctl suspend;;
    	Hibernate)
    	    systemctl hibernate;;
    	Shutdown)
    	    systemctl shutdown;;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    link_menu () {
        opt=$(echo -e "LinkedIn\nPersonal" | fuzzel -d --prompt="Link: " -l 2)
        case $opt in
    	LinkedIn)
    	    wtype "https://www.linkedin.com/in/jonathanmfung";;
    	Personal)
    	    wtype "https://jonathanmfung.github.io";;
    	*)
    	    echo "Invalid option $opt";;
        esac
    }

    main_menu (){
        opt=$(echo -e "Power\nLink" | fuzzel -d --prompt="Menu: " -l 2)
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
