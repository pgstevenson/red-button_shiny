Red Button App
================

## Access database via psql container

    psql -U docker -d red_button

## Initiation

After installing Docker and docker-compose, images can be built and the app
launched by entering this into cmd

    sh \path\to\diccectory\refresh

## Issues

[x] What happens if description has ' or " (or other special characters)
[x] Bug when adding new task, doesn't seem to update with the selected service_id
[x] What happens when update event is clicked with no changes?
[x] App terminates before event has stopped, perpetually NA.
[x] Refresh project list button
[] Possible issue when adding events/projects to users/clients with no pre-existing data - error when adding data, possibly can't bind_rows?

## To Do

[x] Shiny module list events
[] Add Users - select timezone/use client or system time zone as default
[] Add Clients (via app, just skeleton api for now) - select time zone
[] User roles
[] Form validation (i.e. start/end times are correct format)
[] What happens for individual tier? (auto add user to new projects)
[] time to minute in app (strip seconds)
[x] Move time to api with user time zones
[] Roll api event create/stop into single end point

## Longer term features

[] Client tiers
[] Client/user email validation (send verification code that needs to be clicked)
[] Billing
[] Delete event
[] 'active' user status
[] Project manager(s)/admin
[] Edit projects
[] Access/permissions

## Test cases

[] New Client - no data
[] New user (tier >= 1) - no data
[] New user (tier 0) - no data