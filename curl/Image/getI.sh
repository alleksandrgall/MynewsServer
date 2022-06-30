#-c? - credentials
#-i - image id to get

#example: curl/Image/getI.sh -i 1

while getopts ":i:n:p:c:" opt; do
  case $opt in
    i) id="$OPTARG"
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

curl -v --request GET \
  --url "http://localhost:3000/image/${id}" \
  --header "Authorization: Basic `echo -n ${cred} | base64`"