# use this to change the number of passes you want to make on each project
$testRuns = 5
# stops script execution if an error is raised
$ErrorActionPreference = 'Stop'

# used to check for a valid test run
$validateRegEx = "Passes:.*Valid:\sTrue$"
# used to parse output for easier formatting. online tester: https://regex101.com/r/RzFsdr/1
$resultsRegEx = "Passes:\s(?<passes>.+),\sTime:\s(?<time>.+),\sAvg:\s(?<avg>[\d\.]+)\,\sLimit:\s(?<limit>\d+),\sCount:\s(?<count>\d+),\sValid:\s(?<valid>\w+)$"

$testResults = [System.Collections.ArrayList]::new()

# searches for all .csproj files within this repo (to make it easier to test other frameworks/targets/configurations/etc)
$projects = Get-ChildItem -Path $PSScriptRoot -Filter *.csproj -File -Recurse

foreach ($project in $projects)
{
    Write-Host "Running '" -NoNewline
    Write-Host ($project.Name) -ForegroundColor Cyan -NoNewline
    Write-Host "'" -NoNewline

    $projectPath = $project.FullName

    for ($run = 1; $run -le $testRuns; $run++)
    {
        # run the project and save the output in $output for validation/parsing
        $output = [string](dotnet run --project "$projectPath")

        # make sure the output shows validation was successful
        if (!($output -match $validateRegEx)) {
            throw "An error ocurred running '$project'.`r`n$output"
        }

        # use regex to parse the output so we can format the results
        if (!($output -match $resultsRegEx)) {
            throw "Project '$project' ran successfully but results were not in a valid format. See `$resultsRegEx."
        }

        $projectName = [System.IO.Path]::GetFileNameWithoutExtension($project)

        # create an object from the $Matches hashtable (the result of -match $resultsRegEx)
        $result = New-Object PSCustomObject -Property $Matches
        # add custom properties to the object for the project name and the run number
        $result | Add-Member -MemberType NoteProperty -Name project -Value $projectName
        $result | Add-Member -MemberType NoteProperty -Name run -Value $run

        # override the object to not include the '0' property (note, since Format-Table is using -Property this isn't necessary)
        $result = $result | Select-Object project, run, passes, time, avg, limit, count, valid

        # add the custom object to the list of results
        [void]$testResults.Add($result)

        Write-Host "." -NoNewline
    }

    Write-Host
}

$testResults | Sort-Object -Property project,run | Format-Table -AutoSize -Property project,run,passes,time,avg,limit,count,valid
