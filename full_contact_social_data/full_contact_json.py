import requests, json, csv, urllib2, simplejson, collections, time

def getuserinfo( email ):

          mydict = {}
          keys = ['email', 'fullName', 'url', 'locationGeneral','age','ageRange','gender','name','title','startdate','current','bio','typeName','followers','linkedinbio', 'twitterbio', 'angellistbio']
          val = 'NA'
          mydict = {k: val for k in keys}

          mydict['email'] = email

          ## Replace with your key
          API_KEY = '&apiKey=XXXXXX'

          url = 'https://api.fullcontact.com/v2/person.json?email=' + email + API_KEY

          response = urllib2.urlopen(url)
          result = response.read()
          d = json.loads(result)
          
          if d['status'] != 200:
               print 'ERROR. Status: ' + d['status']

          else:

               if 'contactInfo' in d:
                    if 'fullName' in d['contactInfo']:
                         mydict['fullName'] = d['contactInfo']['fullName']
               
               if 'photos' in d:
                    if 'url' in d['photos'][0]:
                         mydict['url'] = d['photos'][0]['url']

               listfielddemographics = ['locationGeneral','age','ageRange','gender']
               for field in listfielddemographics:
                    if 'demographics' in d:
                         if field in d['demographics']:
                              mydict[field] = d['demographics'][field]
                         
               listfieldorganizations = ['name','title','startdate','current']
               for field in listfieldorganizations:
                    if 'organizations' in d:
                         if field in d['organizations'][0]:
                              mydict[field]  = d['organizations'][0][field]
                        
               listfieldsocial = ['bio','typeName','followers']
               for field in listfieldsocial:
                    if 'socialProfiles' in d:
                         if field in d['socialProfiles'][0]:
                              mydict[field]  = d['socialProfiles'][0][field]

               if 'socialProfiles' in d:
                    for p in d['socialProfiles']:

                         if p['type'] == 'linkedin':
                              if 'bio' in p:
                                   mydict['linkedinbio'] = p['bio']

                         else:
                         
                              if p['type'] == 'twitter':
                                   if 'bio' in p:
                                        mydict['twitterbio'] = p['bio']

                              else:
                              
                                   if p['type'] == 'angellist':
                                        if 'bio' in p:
                                             mydict['angellistbio'] = p['bio']

          return mydict


if __name__ == "__main__":

          filename = "YOUR_PATH/emails.csv" # file to read emails from
          fp = open('result.json', 'w')

          total_calls = 0

          f = csv.reader(open(filename, 'rU'))
          for row in f:

               try:

                    email = row[1]
                    print email

                    time.sleep(1)

                    my_dict = getuserinfo(email)
                    
                    total_calls += 1 # Keep count of calls to API because of limit

                    print total_calls
                    print my_dict

                    json.dump(my_dict, fp)

               except: pass

