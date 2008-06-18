#!/usr/bin/env ruby -w

# Scrapes the HTML page with test run summary from CommonTest and emails
# the results.
# <hasan@hypernumbers.com>

require "rubygems"
require "hpricot"
require "net/smtp"
require "smtp_tls"

RECIPIENT = "dev-ml@hypernumbers.com"
SMTP_LOGIN = "build@hypernumbers.com"
SMTP_PASSWORD = "security"

def send_email(from, from_alias, to, to_alias, subject, message)
  msg = <<END_OF_MESSAGE
From: #{from_alias} <#{from}>
To: #{to_alias} <#{to}>
Subject: #{subject}

#{message}
END_OF_MESSAGE

  Net::SMTP.start("mail.hypernumbers.com", 25, "localhost", 
                  SMTP_LOGIN, SMTP_PASSWORD, "plain") { |smtp|
    smtp.send_message(msg, from, to) 
  }
end

doc = open(ARGV[0]) { |f| Hpricot(f) }
stats = (doc/"table/tr")[0..-2].map { |tr|
  tds = (tr/"td")
  [ (tds[0]/"a").inner_html, # name of the test suite
    tds[2].inner_html,       # successful
    tds[3].inner_html,       # failed
    tds[4].inner_html        # skipped
  ]
} << [ "TOTAL",
      (((((doc/"table"/"tr")[-1]/"td")[2])/"b")[0]).inner_html,
      (((((doc/"table"/"tr")[-1]/"td")[3])/"b")[0]).inner_html,
      (((((doc/"table"/"tr")[-1]/"td")[4])/"b")[0]).inner_html
     ]

day, month, year = [Time.now.day, Time.now.month, Time.now.year]
subject = "Nightly test run - #{day}/#{month}/#{year}"

send_email(SMTP_LOGIN, "Buildbot",
           RECIPIENT, "dev-ml",
           subject,
           stats.map { |row| row.join "  " }.join("\n"))
