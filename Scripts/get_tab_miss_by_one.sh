cd /Users/jamesbrown/Projects/NBA
mkdir -p Data/tab_points_miss_by_one_history

git log --since="2025-10-15" --pretty=format:"%H %ci" -- Data/tab_points_miss_by_one.rds | while read hash date time tz; do
    datetime="${date}_${time//:/-}"
    git show "${hash}:Data/tab_points_miss_by_one.rds" > "Data/tab_points_miss_by_one_history/tab_points_miss_by_one_${datetime}.rds" 2>/dev/null
done

ls Data/tab_points_miss_by_one_history | wc -l
