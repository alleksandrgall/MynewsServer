#-n - username
#-c - credentials

#example: curl/User/toAuthor.sh -n NotAnAuthor -c admin:admin

while getopts ":n:c:" opt; do
  case $opt in
    n) name="$OPTARG"
    ;;
    c) cred="$OPTARG"
    ;;
    \?) echo "Invalid option -$OPTARG" >&2
    exit 1
    ;;
  esac

  case $OPTARG in
    -*) echo "Option $opt needs a valid argument"
    exit 1
    ;;
  esac
done

curl -v --request POST -G \
  --url "http://localhost:3000/user/to_author" \
  --data-urlencode "username=$name" \
  --header "Authorization: Basic `echo -n $cred | base64`"