cd /Users/jamesbrown/Projects/NBA
mkdir -p Data/all_arbs_history

git log --since="2025-10-15" --pretty=format:"%H %ci" -- Data/all_arbs.rds | while read hash date time tz; do
    datetime="${date}_${time//:/-}"
    git show "${hash}:Data/all_arbs.rds" > "Data/all_arbs_history/all_arbs_${datetime}.rds" 2>/dev/null
done

ls Data/all_arbs_history | wc -l