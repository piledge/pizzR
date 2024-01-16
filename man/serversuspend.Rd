\name{server.suspend}
\alias{server.suspend}
\title{Function to suspend a Server after processing.}
\usage{
server.suspend(x, y)
}
\arguments{
\item{x}{integer. Gracetime for suspension. Default = 60 Seconds.}
\item{y}{character. Path for execution of suspension command. Need to be executable without root privileges by user.}
}
\description{
Function to suspend a Server after processing.

### helpful Information for suspension on Linux
# - example for suspend.sh
		#!/bin/bash
		systemctl suspend
# - example required user privileges; add in /etc/sudoers.d/suspend; USERNAME is the name of the user claiming suspension privileges
		# Cmnd alias specification
		Cmnd_Alias SYSTEM = /usr/bin/systemctl suspend

		# User privilege specification
		USERNAME ALL=(root) NOPASSWD: SYSTEM
}
\examples{
library(pizzR)

# suspend my server after 5 seconds
server.suspend(5)

# suspend any server after 5 seconds
server.suspend(10, 'sh /path_to_suspension.sh')
}
