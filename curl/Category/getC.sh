#-l? - limit
#-o? - offset
#-c - credentials

#example  curl/User/getU.sh -l 3 -o 4

while getopts ":l:o:c:" opt; do
  case $opt in
    l) limit="$OPTARG"
    ;;
    o) offset="$OPTARG"
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

if [ ! -z $limit ] 
then 
    lim="limit=${limit}&"
else
    lim=""
fi

if [ ! -z $offset ] 
then 
    off="offset=${offset}"
else
    off="" 
fi

curl -v  --request GET \
  --url "http://localhost:3000/category?${lim}${off}" \
  --header "Authorization: Basic `echo -n ${cred:-} | base64`"