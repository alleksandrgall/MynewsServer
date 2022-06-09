#-d - article id
#-i - ids of to be deleted images's
#-c - credentials

#example curl/Article/alterDelete.sh -c Author:AuthorPass -i "69;68;67;70;71;72;73;74;75;76;77;78;79" -d 72

while getopts ":d:t:n:i:p:y:c:" opt; do
  case $opt in
    d) id="$OPTARG"
    ;;
    i) images="$OPTARG"
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

imageQueries=""
imagesAr=$(echo $images | tr ";" "\n")

for image in $imagesAr
do
  imageQueries="$imageQueries --data-urlencode image_id[]=${image}"  
done

curl -v --request DELETE -G\
  --header "Authorization: Basic `echo -n ${cred:-} | base64`" \
  --url "http://localhost:3000/article/alter/${id}" \
  $imageQueries


