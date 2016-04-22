#!/usr/bin/env python3
import mail_config
import sendgrid
import logging
import csv
from random import gauss
from time import sleep


# ------------------
# E-mail variables
# ------------------
deadline = 'April 29, 2016'
subject = 'Help with research about international NGOs'
body = ' '


wait_mu = 0.25
wait_sd = 0.05

# ----------------
# Set up logging
# ----------------
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# Create handler
handler = logging.FileHandler('send_mail.log')
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


if __name__ == '__main__':
    email_orgs = get_emails('list/groups/round_8_clean.csv')
    # email_orgs = [{'id_org': 1, 'email': 'andrewheiss@gmail.com',
    #                'org_name_email': 'Does this go to two people?'}]

    for i, org in enumerate(email_orgs):
        # send_email(org, deadline, subject, log=False)
        logger.info('Dealing with row {0}.'.format(i + 1))
        if i > 0:
            wait = abs(gauss(wait_mu, wait_sd))
            logger.info('Pausing for {0:.2f} seconds.'.format(wait))
            sleep(wait)
        send_email(org, deadline, subject)
