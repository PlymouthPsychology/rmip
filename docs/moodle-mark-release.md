# How to upload and release marks on Moodle (DLE)

_Michael Verde, Julien Besle_

**These steps should only be done by the module leader**. They should also only be done once all marks have been finalised (i.e. post marking and moderation).

## Preliminary steps
1. Click the DLE submission link, then view all submissions.

2. If not already done so: from the Grading Action drop down menu at the top, select "Reveal Student Identities"

3. Click "Continue"

## How to upload marks

1. Download an empty grading sheet: In the "Grading action" drop-down list, select "Download grading worksheet"

2. Save as a local csv file

3. Add the marks to the csv file manually or using an automated script (see the script folder in this repository)

4. In the "Grading action" drop-down list, select "Upload grading worksheet"

5. In "Upload a file", click "Choose a file" and select the csv file containing marks and email addresses

6. In necessary, change the encoding and separator options (defaults are normally fine)

7. Optionally, check "Allow updating records that have been modified more recently in Moodle than in the spreadsheet.", if marks had been modified from within the DLE previously and you want to overwrite them.

8. Click "Upload grading worksheet"

9. Review the modified grades and click "Confirm"

## How to release marks

1. At the top of the "Submissions" page, below the "grading action" drop-down menu, make sure "All" first names and "All" Last names are selected.

3. Scroll to the bottom of the page. Under Options, be sure the "Assignments per page" is set to "All". Now scroll back to the top of the page. Just above where the rows of student records start, on the left, check the box "Select". This will select every student record. 

4. At the bottom of the page, from the "With selected..." drop down menu, choose "Set marking workflow state (Release grades and feedback)"

5. Hit Go. (you will be asked to confirm, then continue)

6. Not done yet! On the next page that appears, from the "marking workflow" drop down menu at the bottom, select "Released"

7. Notify students should remain at "No"

8. Save Changes
