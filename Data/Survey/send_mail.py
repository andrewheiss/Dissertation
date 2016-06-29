#!/usr/bin/env python3
import mail_config
import sendgrid
import requests
import logging
import csv
from random import gauss
from time import sleep


# ------------------
# E-mail variables
# ------------------
deadline = 'July 5, 2016'
subject = 'Help with research about international NGOs'

wait_mu = 0.25
wait_sd = 0.05

# ------------------------
# Load Mailgun templates
# ------------------------
with open('templates/first_reminder.html', 'r') as f:
    # Python's format()-with-{}s is cool, but it chokes on CSS since it also
    # uses curly braces. Rather than double all curly braces in the HTML
    # template (since that would make it invalid HTML), double them here after
    # reading the file. Doing this breaks any actual replacement placeholders,
    # so instead of using {0}, {1}, etc. in the template, use ```0''', ```1''',
    # etc. and replace ``` with { and ''' with } after reading the file.
    template_initial_html = (f.read().replace('{', '{{').replace('}', '}}')
                                     .replace('```', '{').replace("'''", '}'))

with open('templates/first_reminder.txt', 'r') as f:
    template_initial_text = f.read()


# ----------------
# Set up logging
# ----------------
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# Create handler
handler = logging.FileHandler('send_mail_mg.log')
handler.setLevel(logging.INFO)

# Logging format
formatter = logging.Formatter(fmt='%(levelname)-8s %(name)-8s %(funcName)-20s %(asctime)s: %(message)s',
                              datefmt='%Y-%m-%d %H:%M:%S')
handler.setFormatter(formatter)

# Add handler to logger
logger.addHandler(handler)


def get_emails(filename):
    with open(filename) as f:
        orgs_to_email = [{k: v for k, v in row.items()}
                         for row in csv.DictReader(f, skipinitialspace=True)]
    logger.info('E-mail addresses loaded from {0}.'.format(filename))
    return orgs_to_email

def send_email(org, deadline, subject, body=' ', log=True):
    # Connect to API and initialize message
    client = sendgrid.SendGridClient(mail_config.API_KEY)
    message = sendgrid.Mail()

    # Sender details
    message.set_from('survey@ingoresearch.org')
    message.set_from_name('Andrew Heiss')

    # Use template
    message.add_filter('templates', 'enable', '1')
    message.add_filter('templates', 'template_id', mail_config.TEMPLATE_ID)

    # Recipient details + template substitutions
    message.add_to(org['email'])
    message.add_substitution('{org_name}', org['org_name_email'])
    message.add_substitution('{deadline}', deadline)

    # Message details
    message.set_subject(subject)
    # Set both HTML and text for a multipart message
    message.set_html(body)
    message.set_text(body)

    # Send this puppy
    # result = ''
    result = client.send(message)
    if log:
        logger.info('Message sent to {0} ({1}: {2}). Response: {3}.'
                    .format(org['email'], org['id_org'],
                            org['org_name_email'], result))
    else:
        print(result)

def send_email_mg(org, deadline, subject, body=' ', log=True):
    # Create API call
    url = 'https://api.mailgun.net/v3/{0}/messages'.format(mail_config.MG_DOMAIN)
    auth = ('api', mail_config.MG_API_KEY)

    # Build message
    data = {
        'from': 'Andrew Heiss <survey@ingoresearch.org>',
        'to': org['email'],
        'subject': subject,
        'text': template_initial_text.format(org['org_name_email'], deadline),
        'html': template_initial_html.format(org['org_name_email'], deadline)
    }

    # Send this puppy
    response = requests.post(url, auth=auth, data=data)

    if log:
        logger.info('Message sent to {0} ({1}: {2}). Response: {3}: {4}.'
                    .format(org['email'], org['id_org'], org['org_name_email'],
                            response.status_code, response.text.replace('\n', '')))
    else:
        print(response.text)


if __name__ == '__main__':
    # email_orgs = get_emails('list/groups_reminders/group_9_clean.csv')
    # email_orgs = [{'id_org': 1, 'email': 'andrew.heiss@duke.edu',
    #                'org_name_email': 'Amnesty International'}]

    for i, org in enumerate(email_orgs):
        # send_email_mg(org, deadline, subject, log=False)
        logger.info('Dealing with row {0}.'.format(i + 1))
        if i > 0:
            wait = abs(gauss(wait_mu, wait_sd))
            logger.info('Pausing for {0:.2f} seconds.'.format(wait))
            sleep(wait)
        send_email_mg(org, deadline, subject)
