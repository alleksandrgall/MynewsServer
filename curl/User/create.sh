#!/bin/bash

#-n - username
#-a - user avatar path
#-d - should user be admin
#-t - should user be author
#-p - user password
#-c - credentials

#example: curl/User/create.sh -a curl/imagesForTesting/image3.jpg -n CurlUser -p CurlUserPass -d false -t false -c admin:admin


while getopts ":n:a:d:t:p:c:" opt; do
  case $opt in
    n) name="$OPTARG"
    ;;
    a) avatar="$OPTARG"
    ;;
    d) isAdmin="$OPTARG"
    ;;
    t) isAuthor="$OPTARG"
    ;;
    p) pass="$OPTARG"
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

if [ ! -z "${avatar}" ] 
then 
    avatarForm=(--form "avatar=@$avatar")
else
    avatarForm=()
fi

curl -v --request PUT \
--url http://localhost:3000/user/create \
--header "Authorization: Basic `echo -n $cred | base64`" \
--header 'Content-Type: multipart/form-data' \
--header 'content-type: multipart/form-data; boundary=---011000010111000001101001' \
--form "name=$name" \
--form "password=$pass" \
--form "is_admin=$isAdmin" \
--form "is_author=$isAuthor" \
${avatarForm[@]}
