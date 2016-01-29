# set of general utilities to interact with the Web

# @author: rm3086 (at) columbia (dot) edu

import urllib2, json
from log import strd_logger

# logger
log = strd_logger ('web')

# get the html source associated to a URL
def download_web_data(url):
    try:
        req = urllib2.Request(url, headers={'User-Agent':' Mozilla/5.0'})
        con = urllib2.urlopen(req)
        html = con.read()
        con.close()
        return html
    except Exception as e:
        print str(e) + 'Exception occur at : ' + url
        return None

# get json data
def download_json_data (url):
    try:
        con = urllib2.urlopen(url)
        data = con.read()
        con.close()
        return json.loads(data)
    except Exception as e:
        log.error ('%s: %s' % (e, url))
        return None


# clean the html source
def clean_html (html):
	if html is None:
		return None
	return ' '.join(html.replace('\n','').replace('\t','').split()).strip()

import re, htmlentitydefs

##
# Removes HTML or XML character references and entities from a text string.
#
# @param text The HTML (or XML) source text.
# @return The plain text, as a Unicode string, if necessary.

def unescape(text):
    def fixup(m):
        text = m.group(0)
        if text[:2] == "&#":
            # character reference
            try:
                if text[:3] == "&#x":
                    return unichr(int(text[3:-1], 16))
                else:
                    return unichr(int(text[2:-1]))
            except ValueError:
                pass
        else:
            # named entity
            try:
                text = unichr(htmlentitydefs.name2codepoint[text[1:-1]])
            except KeyError:
                pass
        return text # leave as is
    return re.sub("&#?\w+;", fixup, text)