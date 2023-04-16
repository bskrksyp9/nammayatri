
export function getStringValue(key) {
  if (key in mlStrings) {
    return mlStrings[key];
  }
  console.error("string not found in mlStrings");
  return "";
}

const mlStrings = {
  "LETS_GET_STARTED": "ആരംഭിക്കുക",
  "YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION": "നിങ്ങളുടെ അപേക്ഷ വിജയകരമായി സമർപ്പിക്കപ്പെട്ടു. ഞങ്ങൾ അത് തുടർന്നു സ്ഥിതികരിക്കുകയാണ് ",
  "VIEW_STATUS": "സ്റ്റാറ്റസ് അറിയുക",
  "GO_HOME": "ഹോമിലേക്ക് പോകുക",
  "SELECT_LANGUAGE": "ഭാഷ തിരഞ്ഞെടുക്കുക",
  "WHICH_LANGUAGE_DO_YOU_PREFER": "ഏത് ഭാഷയ്ക്കാണ് നിങ്ങൾ മുൻഗണന നൽകുന്നത്?",
  "NEXT": "അടുത്തത്",
  "T_C": "നിബന്ധനകളും വ്യവസ്ഥകളും",
  "ENTER_MOBILE_NUMBER": "മൊബൈൽ നമ്പർ നൽകുക",
  "BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR": "അടുത്ത ടാപ്പുചെയ്യുന്നതിലൂടെ \n a) നിങ്ങൾ ബീറ്റ ടെസ്റ്റിന് പങ്കെടുക്കാൻ സമ്മതം രേഖപെടുത്തുന്നു.ജസ്പേയ്ക്ക് യാതൊരു വിധ ബാധ്യതയുമില്ലെന്ന് നിങ്ങൾ സമ്മതിക്കുന്നു.",
  "ENTER_OTP": "OTP നൽകുക",
  "DIDNT_RECIEVE_OTP": "OTP കിട്ടിയില്ലേ?",
  "RESEND_OTP": "OTP വീണ്ടും അയയ്ക്കുക",
  "PLEASE_ENTER_VALID_OTP": "സാധുവായ OTP നൽകുക",
  "INVALID_MOBILE_NUMBER": "അസാധുവായ മൊബൈൽ നമ്പർ",
  "REGISTER": "രജിസ്റ്റർ ചെയ്യുക",
  "MOBILE_NUMBER": "മൊബൈൽ നമ്പർ",
  "AUTO_READING_OTP": "OTP ഓട്ടോ-റീഡ് ചെയ്യുന്നു…",
  "UPLOAD_DRIVING_LICENSE": "ഡ്രൈവിംഗ് ലൈസൻസ് അപ്ലോഡു ചെയ്യുക",
  "UPLOAD_BACK_SIDE": "DL- ന്റെ പിൻവശം അപ്ലോഡ് ചെയ്യുക",
  "UPLOAD_FRONT_SIDE": "DL- ന്റെ മുൻവശം  അപ്ലോഡ് ചെയ്യുക",
  "BACK_SIDE": "പിൻവശം",
  "FRONT_SIDE": "മുൻവശം",
  "LICENSE_INSTRUCTION_PICTURE": "ദയവായി ലൈസൻസിന്റെ ഇരുവശവും അപ്ലോഡുചെയ്യുക",
  "LICENSE_INSTRUCTION_CLARITY": "ഫോട്ടോയും എല്ലാ വിശദാംശങ്ങളും വ്യക്തമായി കാണാൻപറ്റുമെന്നു ഉറപ്പാക്കുക",
  "REGISTRATION_STEPS": "രജിസ്ട്രേഷൻ ഘട്ടങ്ങൾ",
  "PROGRESS_SAVED": "നിങ്ങളുടെ പുരോഗതി സേവ് ചെയ്തു, ഏതെങ്കിലും വിവരങ്ങൾ മാറ്റുന്നതിന് നിങ്ങൾക്ക് മുമ്പത്തെ ഘട്ടങ്ങളിലേക്കും മടങ്ങാം",
  "DRIVING_LICENSE": "ഡ്രൈവിംഗ് ലൈസൻസ്",
  "AADHAR_CARD": "ആധാർ കാർഡ്",
  "BANK_DETAILS": "ബാങ്ക് വിശദാംശങ്ങൾ",
  "VEHICLE_DETAILS": "വാഹന വിശദാംശങ്ങൾ",
  "UPLOAD_FRONT_BACK": "ഇരുവശത്തിന്റെയും ചിത്രങ്ങൾ ദയവായി അപ്ലോഡുചെയ്യുക",
  "EARNINGS_WILL_BE_CREDITED": "നിങ്ങളുടെ വരുമാനം ഇവിടെ ക്രെഡിറ്റ് ചെയ്യും",
  "FILL_VEHICLE_DETAILS": "നിങ്ങളുടെ വാഹന വിശദാംശങ്ങൾ പൂരിപ്പിക്കുക",
  "FOLLOW_STEPS": "രജിസ്റ്റർ ചെയ്യുന്നതിന് ദയവായി ചുവടെയുള്ള നടപടികൾ പാലിക്കുക",
  "REGISTRATION": "രജിസ്ട്രേഷൻ",
  "UPLOAD_ADHAAR_CARD": "ആധാർ കാർഡ് അപ്ലോഡുചെയ്യുക",
  "ADHAAR_INTRUCTION_PICTURE": "ആധാർ കാർഡിന്റെ ഇരുവശത്തിന്റെയും ചിത്രങ്ങൾ ദയവായി അപ്ലോഡുചെയ്യുക",
  "ADD_VEHICLE_DETAILS": "വാഹന വിശദാംശങ്ങൾ ചേർക്കുക",
  "VEHICLE_REGISTRATION_NUMBER": "വാഹന രജിസ്ട്രേഷൻ നമ്പർ",
  "RE_ENTER_VEHICLE_REGISTRATION_NUMBER": "വാഹന രജിസ്ട്രേഷൻ നമ്പർ വീണ്ടും നൽകുക",
  "ENTER_VEHICLE_NO": "വാഹന നമ്പർ നൽകുക.",
  "VEHICLE_TYPE": "വാഹന തരം",
  "VEHICLE_MODEL_NAME": "വാഹന മോഡലിന്റെ പേര്",
  "ENTER_MODEL_NAME": "മോഡൽ പേര് നൽകുക",
  "VEHICLE_COLOUR": "വാഹനത്തിന്റെ നിറം",
  "ENTER_VEHICLE_COLOUR": "വാഹനത്തിന്റെ നിറം നൽകുക",
  "UPLOAD_REGISTRATION_CERTIFICATE": "രജിസ്ട്രേഷൻ സർട്ടിഫിക്കറ്റ് അപ്ലോഡുചെയ്യുക (ആർസി)",
  "UPLOAD_RC": "ആർസി അപ്ലോഡുചെയ്യുക",
  "PREVIEW": "പ്രിവ്യൂ",
  "CHOOSE_VEHICLE_TYPE": "വാഹന തരം തിരഞ്ഞെടുക്കുക",
  "BENIFICIARY_NUMBER": "ഗുണഭോക്തൃ അക്കൗണ്ട് നമ്പർ",
  "RE_ENTER_BENIFICIARY_NUMBER": "ഗുണഭോക്തൃ അക്കൗണ്ട് നമ്പർ വീണ്ടും നൽകുക.",
  "IFSC_CODE": "Ifsc കോഡ്",
  "SENDING_OTP": "OTP അയയ്ക്കുന്നു",
  "PLEASE_WAIT_WHILE_IN_PROGRESS": "പുരോഗതിയിൽ; ദയവായി കാത്തിരിക്കുക",
  "LIMIT_EXCEEDED": "പരിധി കവിഞ്ഞു",
  "YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN": "നിങ്ങളുടെ അഭ്യർത്ഥനയുടെ സമയപരിധി അവസാനിച്ചു. വീണ്ടും ശ്രമിക്കുക.",
  "ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER": "പിശക് സംഭവിച്ചിട്ടുണ്ട്; ദയവായി അല്പസമയത്തിനുള്ളിൽ വീണ്ടും ശ്രമിക്കുക",
  "LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN": "പരിധി കവിഞ്ഞു; ദയവായി അല്പസമയത്തിനുള്ളിൽ വീണ്ടും ശ്രമിക്കുക",
  "ENTER_OTP_SENT_TO": "OTP നൽകുക; OTP അയച്ച നമ്പർ:",
  "OTP_SENT_TO": "",
  "COUNTRY_CODE_INDIA": "91",
  "ENTER_ACCOUNT_NUMBER": "അക്കൗണ്ട് നമ്പർ നൽകുക.",
  "ADD_BANK_DETAILS": "ബാങ്ക് വിശദാംശങ്ങൾ ചേർക്കുക",
  "ENTER_IFSC_CODE": "IFSC കോഡ് നൽകുക",
  "SUBMIT": "സമർപ്പിക്കുക",
  "PERSONAL_DETAILS": "വ്യക്തിഗത വിശദാംശങ്ങൾ",
  "LANGUAGES": "ഭാഷകൾ",
  "HELP_AND_FAQ": "സഹായവും പതിവുചോദ്യങ്ങളും",
  "ABOUT": "ആപ്പിനെ കുറിച്ച്",
  "LOGOUT": "ലോഗൗട്ട്",
  "UPDATE": "അപ്ഡേറ്റ് ചെയ്യുക",
  "EDIT": "എഡിറ്റ് ചെയ്യുക",
  "AUTO": "ഓട്ടോ",
  "NAME": "പേര്",
  "PRIVACY_POLICY": "സ്വകാര്യതാ നയം",
  "LOGO": "ലോഗോ",
  "ABOUT_APP_DESCRIPTION": "യാത്രക്കാരെ ഡ്രൈവർമാരുമായി ബന്ധിപ്പിക്കുന്നതിനുള്ള ഒരു ഓപ്പൺ പ്ലാറ്റ്ഫോമാണ് നമ്മ യാത്രി പാർട്ട്നർ. നിർദ്ദിഷ്ട നിരക്കുകളിൽ റൈഡറുകളെ കണ്ടെത്താൻ ഡ്രൈവർമാർക്ക് അപ്ലിക്കേഷൻ സൗകര്യപ്രദമാക്കും. റൈഡ് അടിസ്ഥാനമാക്കിയുള്ള കമ്മീഷനൊന്നും തന്നെ ഇല്ല, പ്രതിമാസ സബ്സ്ക്രിപ്ഷന്റെ രൂപത്തിൽ ചെറിയ തുക മാത്രമേ അടക്കേണ്ടതൊള്ളൂ",
  "TERMS_AND_CONDITIONS": "നിബന്ധനകളും വ്യവസ്ഥകളും",
  "UPDATE_VEHICLE_DETAILS": "വാഹന വിശദാംശങ്ങൾ അപ്ഡേറ്റുചെയ്യുക",
  "Help_AND_SUPPORT": "സഹായസഹകരണങ്ങൾ",
  "NOTE": "കുറിപ്പ്:",
  "VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS": "നിർദ്ദിഷ്ട പരാതികൾക്കായി എന്റെ റൈഡുകൾ വിഭാഗം സന്ദർശിക്കുക",
  "THANK_YOU_FOR_WRTITTING_US": "ഞങ്ങൾക്ക് എഴുതിയതിന് നന്ദി!",
  "WE_HAVE_RECIEVED_YOUR_ISSUE": "നിങ്ങളുടെ പ്രശ്നം ഞങ്ങൾക്ക് ലഭിച്ചു. ഞങ്ങൾ കുറച്ച് സമയത്തിനുള്ളിൽ നിങ്ങളെ ബന്ധപ്പെടും.",
  "GO_TO_HOME": "ഹോം -ലേയ്ക്ക് പോകൂ",
  "YOUR_RECENT_RIDE": "സമീപകാലത്തെ നിങ്ങളുടെ റൈഡ്",
  "ALL_TOPICS": "എല്ലാ വിഷയങ്ങളും",
  "REPORT_AN_ISSUE_WITH_THIS_TRIP": "ഈ യാത്രയെ സംബന്ധിച്ച് ഒരു പ്രശ്നം  റിപ്പോർട്ടുചെയ്യുക",
  "YOU_RATED": "നിങ്ങൾ റേറ്റു ചെയ്തു:",
  "VIEW_ALL_RIDES": "എല്ലാ റൈഡുകളും കാണുക",
  "WRITE_TO_US": "ഞങ്ങൾക്ക് എഴുതുക",
  "SUBJECT": "വിഷയം",
  "YOUR_EMAIL_ID": "നിങ്ങളുടെ ഇമെയിൽ ഐഡി",
  "DESCRIBE_YOUR_ISSUE": "നിങ്ങളുടെ പ്രശ്നത്തെക്കുറിച്ച് വിവരിക്കുക",
  "GETTING_STARTED_AND_FAQ": "ആരംഭവും പതിവുചോദ്യങ്ങളും",
  "FOR_OTHER_ISSUES_WRITE_TO_US": "മറ്റ് പ്രശ്നങ്ങൾക്കായി ഞങ്ങൾക്ക് എഴുതുക",
  "CALL_SUPPORT_CENTER": "പിന്തുണാ കേന്ദ്രത്തെ വിളിക്കുക",
  "YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE": "നിങ്ങളുടെ പ്രശ്നം ഇവിടെ വിവരിക്കാം",
  "REGISTRATION_CERTIFICATE_IMAGE": "രജിസ്ട്രേഷൻ സർട്ടിഫിക്കറ്റ് (ആർസി) ചിത്രം",
  "HOME": "ഹോം",
  "RIDES": "റൈഡുകൾ",
  "PROFILE": "പ്രൊഫൈൽ",
  "ENTER_DRIVING_LICENSE_NUMBER": "ഡ്രൈവിംഗ് ലൈസൻസ് നമ്പർ നൽകുക",
  "WHERE_IS_MY_LICENSE_NUMBER": "എന്റെ ലൈസൻസ് നമ്പർ എവിടെയാണ്?",
  "TRIP_DETAILS": "യാത്ര വിശദാംശങ്ങൾ",
  "BY_CASH": "പണമായി",
  "ONLINE_": "ഓൺലൈൻ",
  "REPORT_AN_ISSUE": "ഒരു പ്രശ്നം റിപ്പോർട്ടുചെയ്യുക",
  "DISTANCE": "അകലം",
  "TIME_TAKEN": "എടുത്ത സമയം",
  "OPEN_GOOGLE_MAPS": "ഗൂഗിൾ മാപ് തുറക്കുക",
  "CALL": "കാൾ",
  "START_RIDE": "റൈഡ് ആരംഭിക്കുക",
  "CANCEL_RIDE": "റൈഡ് റദ്ദാക്കുക",
  "PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL": "എന്തുകൊണ്ടാണ് നിങ്ങൾ റൈഡ് റദ്ദാക്കാൻ ആഗ്രഹിക്കുന്നതെന്ന് ഞങ്ങളോട് പറയുക",
  "MANDATORY": "നിർബന്ധമാണ്",
  "END_RIDE": "റൈഡ് അവസാനിപ്പിക്കുക",
  "RIDE_COMPLETED_WITH": "യാത്ര പൂർണമായി; ഉപഭോക്താവ്:",
  "COLLECT_AMOUNT_IN_CASH": "തുക പണമായി കൈപ്പറ്റുക",
  "CASH_COLLECTED": "പണം കൈപറ്റി",
  "OFFLINE": "ഓഫ്ലൈൻ",
  "ACCEPT_FOR": "ഈ നിരക്കിൽ സ്വീകരിക്കുക:",
  "DECLINE": "നിരസിക്കുക",
  "REQUEST": "അപേക്ഷിക്കുക",
  "YOU_ARE_OFFLINE": "നിങ്ങൾ ഓഫ്ലൈനാണ്",
  "YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS": "നിങ്ങൾ നിലവിൽ തിരക്കിലാണ്. യാത്രാ അഭ്യർത്ഥനകൾ സ്വീകരിക്കുന്നതിന് ഓൺലൈനിൽ പോകുക",
  "GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE": "ഓഫ്‌ലൈൻ ആയാൽ നിങ്ങള്ക്ക് റൈഡുകൾ ലഭിക്കില്ല ",
  "CANCEL": "റദ്ദാക്കുക",
  "GO_OFFLINE": "ഓഫ്‌ലൈൻ ആകുക",
  "IS_WAITING_FOR_YOU": "നിങ്ങൾക്കായി കാത്തിരിക്കുന്നു",
  "YOU_ARE_ON_A_RIDE": "നിങ്ങൾ ഒരു റൈഡിലാണ് ...",
  "PLEASE_ASK_RIDER_FOR_THE_OTP": "OTP-ക്കായി റീഡറിനോട് ചോദിക്കുക",
  "COMPLETED_": "പൂർത്തിയായി",
  "CANCELLED_": "റദ്ദായി",
  "WE_NEED_SOME_ACCESS": "ഞങ്ങൾക്ക് അക്സസ്സ് നൽകാൻ അഭ്യർത്ഥിക്കുന്നു",
  "ALLOW_ACCESS": "അക്സസ്സ് അനുവദിക്കുക",
  "THANK_YOU_FOR_WRITING_TO_US": "ഞങ്ങൾക്ക് എഴുതിയതിന് നന്ദി!",
  "RIDER": "റൈഡർ",
  "TRIP_ID": "യാത്രാ ഐഡി",
  "NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST": "അപ്ലിക്കേഷൻ പശ്ചാത്തലത്തിൽ ആയിരിക്കുമ്പോഴും, വരുന്ന റൈഡ് റിക്വസ്റ്റ് കാണുവാൻ സാധിക്കുക",

  "NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP": "ആപ്പ് ദീർഘനേരം പശ്ചാത്തലത്തിൽ പ്രവർത്തിക്കാൻ അനുവദിക്കുന്നു. ശുപാർശ ചെയ്യപ്പെട്ടത്",
  "NEED_IT_TO_AUTOSTART_YOUR_APP": "അപ്ലിക്കേഷൻ പശ്ചാത്തലത്തിൽ പ്രവർത്തിപ്പിക്കുന്നതിലൂടെ സഹായിക്കുന്നു",
  "NEED_IT_TO_ENABLE_LOCATION": "അപ്ലിക്കേഷൻ ഉപയോഗത്തിൽ ഇല്ലാത്തപ്പോൾ പോലും  , നിങ്ങളുടെ നിലവിലെ സ്ഥാനം നിർണ്ണയിക്കാൻ നമ്മ യാത്രി പാർട്ട്നർ ആപ്പ് ലൊക്കേഷൻ ഡാറ്റ ശേഖരിക്കുന്നതായിരിക്കും.",
  "OVERLAY_TO_DRAW_OVER_APPLICATIONS": "മറ്റു അപ്ലിക്കേഷനുകളുടെ മുകളിലൂടെ കാണിക്കുവാൻ അനുവദിക്കുക",

  "BATTERY_OPTIMIZATIONS": "ബാറ്ററി ഒപ്റ്റിമൈസേഷൻ",
  "AUTO_START_APPLICATION_IN_BACKGROUND": "പശ്ചാത്തലത്തിൽ അപ്ലിക്കേഷൻ ഓട്ടോസ്റ്റാർട്ട് ചെയ്യുക",
  "LOCATION_ACCESS": "ലൊക്കേഷൻ ആക്സസ്",
  "ENTER_RC_NUMBER": "ആർസി നമ്പർ നൽകുക",
  "WHERE_IS_MY_RC_NUMBER": "എന്റെ ആർസി നമ്പർ എവിടെയാണ്?",
  "STEP": "ഘട്ടം",
  "PAID": "തുക അടച്ചു",
  "ENTERED_WRONG_OTP": "തെറ്റായ OTP നൽകി",
  "COPIED": "പകർത്തി",
  "BANK_NAME": "ബാങ്കിന്റെ പേര്",
  "AADHAR_DETAILS": "ആധാർ വിശദാംശങ്ങൾ",
  "AADHAR_NUMBER": "ആധാർ നമ്പർ",
  "FRONT_SIDE_IMAGE": "മുൻവശത്തെ ചിത്രം",
  "BACK_SIDE_IMAGE": "പിൻവശത്തെ ചിത്രം",
  "STILL_NOT_RESOLVED": "ഇപ്പോഴും പരിഹരിക്കപ്പെട്ടിട്ടില്ലെങ്കിൽ ഞങ്ങളെ വിളിക്കൂ",
  "CASE_TWO": "b)",
  "NON_DISCLOUSER_AGREEMENT": "നോൺ ഡിസ്‌ക്ലോഷർ കരാർ (എൻഡിഎ)",
  "DATA_COLLECTION_AUTHORITY": "c) തുടരുന്നതിലൂടെ, ഞാൻ ഇതിനാൽ ജസ്പേയെ എന്റെ വിവരങ്ങൾ ശേഖരിക്കുന്നതിന് നിയമിക്കുകയും അംഗീകാരം നൽകുകയും ചെയ്യുന്നു. ഉപയോഗ നിബന്ധനകളും സ്വകാര്യതാ നയവും ഞാൻ അംഗീകരിക്കുന്നു.",
  "SOFTWARE_LICENSE": "സോഫ്റ്റ്വെയർ ലൈസൻസ്",
  "LOAD_MORE": "കൂടുതൽ ലോഡു ചെയ്യുക",
  "ARE_YOU_SURE_YOU_WANT_TO_LOGOUT": "നിങ്ങൾക്ക് ലോഗൗട്ട് ചെയ്യണമെന്ന് ഉറപ്പാണോ?",
  "GO_BACK": "മടങ്ങിപ്പോവുക",
  "THANK_YOU_FOR_REGISTERING_US": "ഞങ്ങളുമായി രജിസ്റ്റർ ചെയ്തതിന് നന്ദി!",
  "UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU": "നിർഭാഗ്യവശാൽ, ഞങ്ങളുടെ സേവനം ഇപ്പോൾ നിങ്ങൾക്കായി ലഭ്യമല്ല. ഞങ്ങൾ നിങ്ങളെ ഉടൻ അറിയിക്കും.",
  "ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE": "സവാരി അവസാനിപ്പിക്കാൻ നിങ്ങൾ ആഗ്രഹിക്കുന്നുണ്ടോ?",
  "EMPTY_RIDES": "റൈഡുകൾ ഇതുവരെ ഇല്ല",
  "YOU_HAVE_NOT_TAKEN_A_TRIP_YET": "നിങ്ങൾ ഇതുവരെ ഒരു യാത്ര നടത്തിയിട്ടില്ല",
  "BOOK_NOW": "ഇപ്പോൾ ബുക്ക് ചെയ്യു",
  "RESEND_OTP_IN": "OTP  വീണ്ടും അയയ്ക്കുക",
  "WE_NEED_ACCESS_TO_YOUR_LOCATION": "ഞങ്ങൾക്ക് നിങ്ങളുടെ ലൊക്കേഷൻ ആക്സസ് ആവശ്യമാണ്!",
  "YOUR_LOCATION_HELPS_OUR_SYSTEM": "നിങ്ങളുടെ ലൊക്കേഷൻ, വേഗത്തിൽ സവാരി കണ്ടെത്താൻ ഞങ്ങളുടെ സിസ്റ്റത്തെ സഹായിക്കുന്നു.",
  "NO_INTERNET_CONNECTION": "ഇന്റർനെറ്റ് കണക്ഷൻ ഇല്ല",
  "PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN": "ദയവായി ഇന്റർനെറ്റ് കണക്ഷൻ പരിശോധിച്ച് വീണ്ടും ശ്രമിക്കുക",
  "TRY_AGAIN": "വീണ്ടും ശ്രമിക്കുക",
  "GRANT_ACCESS": " ആക്സസ് നൽകുക",
  "YOUR_LIMIT_EXCEEDED_TRY_AGAIN_AFTER_10_MIN": "നിങ്ങളുടെ പരിധി കഴിഞ്ഞു, 10 മിനിറ്റ് കഴിഞ്ഞ് വീണ്ടും ശ്രമിക്കുക",
  "ENTER_REFERRAL_MOBILE_NUMBER": "റഫറൽ മൊബൈൽ നമ്പർ നൽകുക",
  "APPLY": "അപേക്ഷിക്കുക",
  "HAVE_A_REFERRAL": "റഫറൽ ഉണ്ടോ?",
  "ADD_HERE": "ഇവിടെ ചേർക്കുക",
  "REFERRAL_APPLIED": "റഫറൽ ഉപയോഗിച്ചു!",
  "SMALLEDIT": "തിരുത്തുക",
  "ADD_DRIVING_LICENSE": "ഡ്രൈവിംഗ് ലൈസൻസ് ചേർക്കുക",
  "HELP": "സഹായം?",
  "INVALID_DL_NUMBER": "DL നമ്പർ അസാധുവാണ്",
  "DRIVING_LICENSE_NUMBER": "ഡ്രൈവിംഗ് ലൈസൻസ് നമ്പർ",
  "RE_ENTER_DRIVING_LICENSE_NUMBER": "ഡ്രൈവിംഗ് ലൈസൻസ് നമ്പർ വീണ്ടും നൽകുക",
  "ENTER_DL_NUMBER": "DL നമ്പർ നൽകുക",
  "SELECT_DATE_OF_BIRTH": "ജനനത്തീയതി തിരഞ്ഞെടുക്കുക",
  "DATE_OF_BIRTH": "ജനന തീയതി",
  "WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION": "രെജിസ്ട്രേഷനെ പറ്റി എളുപ്പത്തിൽ പഠിക്കാൻ  ഒരു ട്യൂട്ടോറിയൽ കാണുക",
  "ENTER_MINIMUM_FIFTEEN_CHARACTERS": "കുറഞ്ഞത്  15 അക്ഷരങ്ങൾ നൽകുക",
  "ADD_YOUR_FRIEND": "നിങ്ങളുടെ സുഹൃത്തിനെ ചേർക്കുക",
  "PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE": "ചിത്രം സ്ഥിരീകരിക്കുന്നത് വരെ  ദയവായി കാത്തിരിക്കുക",
  "VALIDATING": "സ്ഥിരീകരിക്കുന്നു",
  "VERIFICATION_PENDING": "സ്ഥിരീകരണം കഴിഞ്ഞിട്ടില്ല",
  "VERIFICATION_FAILED": "സ്ഥിരീകരണം പരാജയപ്പെട്ടു",
  "NO_DOC_AVAILABLE": "ഡോക്യുമെന്റ് ലഭ്യമല്ല",
  "ISSUE_WITH_DL_IMAGE": "നിങ്ങളുടെ DL ചിത്രത്തിനു കുറച്ച് പ്രശ്നങ്ങളുണ്ടെന്ന് തോന്നുന്നു, ഞങ്ങളുടെ സപ്പോർട്ട് ടീം ഉടൻ നിങ്ങളെ ബന്ധപ്പെടും.",
  "STILL_HAVE_SOME_DOUBT": "ഇപ്പോഴും എന്തെങ്കിലും സംശയമുണ്ടോ?",
  "ISSUE_WITH_RC_IMAGE": "നിങ്ങളുടെ  ആർസി ചിത്രത്തിനു കുറച്ച് പ്രശ്നങ്ങളുണ്ടെന്ന് തോന്നുന്നു, ഞങ്ങളുടെ സപ്പോർട്ട് ടീം ഉടൻ നിങ്ങളെ ബന്ധപ്പെടും.",
  "PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT": "ഡോക്യൂമെന്റിന്റെ ചിത്രം സാധുവാണോ എന്നു പരിശോധിക്കുക     ",
  "OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED": "ക്ഷമിക്കണം! നിങ്ങളുടെ അപേക്ഷ നിരസിക്കപ്പെട്ടു. ദയവായി വീണ്ടും ശ്രമിക്കുക",
  "INVALID_DRIVING_LICENSE": "ഡ്രൈവിംഗ് ലൈസൻസ് അസാധുവാണ്",
  "LIMIT_EXCEEDED_FOR_DL_UPLOAD": "DL അപ്ലോഡിനായുള്ള പരിധി കഴിഞ്ഞു ",
  "INVALID_VEHICLE_REGISTRATION_CERTIFICATE": "വാഹന രജിസ്ട്രേഷൻ സർട്ടിഫിക്കറ്റ് അസാധുവാണ്",
  "LIMIT_EXCEEDED_FOR_RC_UPLOAD": "ആർസി അപ്ലോഡിനായുള്ള പരിധി കഴിഞ്ഞു",
  "YOUR_DOCUMENTS_ARE_APPROVED": "നിങ്ങളുടെ ഡോക്യൂമെന്റസ് അംഗീകരിച്ചു. സപ്പോർട്ട് ടീം നിങ്ങളുടെ അക്കൗണ്ടിനെ ഉടൻ സജീവം ആക്കും. നിങ്ങളുടെ അക്കൗണ്ട് പ്രവർത്തനക്ഷമമാക്കുന്നതിന് നിങ്ങൾക്ക് സപ്പോർട്ട് ടീമിനെ വിളിക്കാം",
  "APPLICATION_STATUS": "അപേക്ഷയുടെ സ്റ്റാറ്റസ്",
  "FOR_SUPPORT": "പിന്തുണയ്ക്കായി",
  "CONTACT_US": " ഞങ്ങളെ സമീപിക്കുക",
  "IMAGE_VALIDATION_FAILED": "ഇമേജ് സ്ഥിരീകരണം പരാജയപ്പെട്ടു",
  "IMAGE_NOT_READABLE": "ചിത്രം വായിക്കാൻ കഴിയില്ല",
  "IMAGE_LOW_QUALITY": "ചിത്രത്തിന്റെ നിലവാരം നല്ലതല്ല",
  "IMAGE_INVALID_TYPE": "നൽകിയ ചിത്രം അസാധുവാണ്",
  "IMAGE_DOCUMENT_NUMBER_MISMATCH": "ഈ ചിത്രത്തിലെ ഡോക്യുമെന്റ് നമ്പർ, തന്ന വിവരവും ആയി പൊരുത്തപ്പെടുന്നില്ല",
  "IMAGE_EXTRACTION_FAILED": "ചിത്രം സ്വീകരിക്കാൻ പരാജയപ്പെട്ടു",
  "IMAGE_NOT_FOUND": "ചിത്രം കണ്ടെത്തിയില്ല",
  "IMAGE_NOT_VALID": " ചിത്രം അസാധുവാണ്",
  "DRIVER_ALREADY_LINKED": "ഡോക്യുമെന്റ് മറ്റൊരു ഡ്രൈവറുമായി ഇതിനകം ബന്ധപ്പെട്ടിരിക്കുന്നു",
  "DL_ALREADY_UPDATED": "മാറ്റങ്ങൾ ഒന്നും  ആവശ്യമില്ല. ഡ്രൈവർ ലൈസൻസ് ഇതിനകം ഡ്രൈവറുമായി ബന്ധപ്പെട്ടിരിക്കുന്നു",
  "RC_ALREADY_LINKED": "വെഹിക്കിൾ ആർസി ലഭ്യമല്ല. മറ്റൊരു ഡ്രൈവറുമായി ഇതിനകം ബന്ധപ്പെട്ടിരിക്കുന്നു",
  "RC_ALREADY_UPDATED": "മാറ്റങ്ങൾ ഒന്നും  ആവശ്യമില്ല. വാഹനം ഇതിനകം ഡ്രൈവറുമായി ബന്ധപ്പെട്ടിരിക്കുന്നു",

  "DL_ALREADY_LINKED": "ഡ്രൈവർ ലൈസൻസ് ലഭ്യമല്ല. മറ്റൊരു ഡ്രൈവറുമായി ഇതിനകം ബന്ധപ്പെട്ടിരിക്കുന്നു",
  "SOMETHING_WENT_WRONG": "എന്തോ കുഴപ്പം സംഭവിച്ചു",
  "PICKUP": "പിക്കപ്പ്",
  "TRIP": "യാത്ര",
  "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "നിലവിൽ, കർണാടകയിൽ രജിസ്റ്റർ ചെയ്ത നമ്പർ മാത്രമേ അനുവദനീയമായുള്ളു",
  "UPDATED_AT": "അപ്ഡേറ്റ് ചെയ്ത സമയം :",
  "TRIP_COUNT": "ഇന്നത്തെ യാത്രകൾ",
  "TODAYS_EARNINGS": "ഇന്നത്തെ വരുമാനം",
  "DATE_OF_REGISTRATION": "രജിസ്ട്രേഷൻ തീയതി",
  "SELECT_DATE_OF_ISSUE": "പ്രശ്നത്തിന്റെ തീയതി തിരഞ്ഞെടുക്കുക",
  "DATE_OF_ISSUE": " ഇഷ്യൂ ചെയ്‍ത തീയതി",
  "PROVIDE_DATE_OF_ISSUE_TEXT": "ക്ഷമിക്കണം, നിങ്ങളുടെ വിശദാംശങ്ങൾ സാധൂകരിക്കാൻ കഴിഞ്ഞില്ല. ലൈസെൻസ് വിശദാംശങ്ങൾ സാധൂകരിക്കുന്നതിനായി<b> ഇഷ്യു ചെയ്ത തീയതി </ b> നൽകുക.",
  "PROVIDE_DATE_OF_REGISTRATION_TEXT": "ക്ഷമിക്കണം, നിങ്ങളുടെ വിശദാംശങ്ങൾ സാധൂകരിക്കാൻ കഴിഞ്ഞില്ല. വാഹന വിശദാംശങ്ങൾ സാധൂകരിക്കുന്നതിനായി<b> രെജിസ്ട്രേഷൻ ചെയ്ത തീയതി </ b> നൽകുക.",
  "SELECT_DATE_OF_REGISTRATION": "രജിസ്ട്രേഷൻ തീയതി തിരഞ്ഞെടുക്കുക",
  "SAME_REENTERED_RC_MESSAGE": "വീണ്ടും നൽകിയ ആർസി നമ്പർ, മുകളിൽ ഉള്ള ആർസി നമ്പറിന് തുല്യമാണെന്ന് ഉറപ്പാക്കുക",
  "SAME_REENTERED_DL_MESSAGE": "വീണ്ടും നൽകിയ ഡി എൽ നമ്പർ, മുകളിൽ ഉള്ള ഡി എൽ നമ്പറിന് തുല്യമാണെന്ന് ഉറപ്പാക്കുക",
  "WHERE_IS_MY_ISSUE_DATE": "എന്റെ ഇഷ്യു ചെയ്ത തീയതി എവിടെയാണ്?",
  "WHERE_IS_MY_REGISTRATION_DATE": "എന്റെ രജിസ്ട്രേഷൻ തീയതി എവിടെയാണ്?",
  "OTP_RESENT": "ഒടിപി വീണ്ടും അയച്ചിരിക്കുന്നു",
  "EARNINGS_CREDITED_IN_ACCOUNT": "നിങ്ങളുടെ വരുമാനം ഈ അക്കൗണ്ടിൽ ക്രെഡിറ്റ് ചെയ്തു",
  "INVALID_PARAMETERS": "അസാധുവായ വിവരങ്ങൾ ",
  "UNAUTHORIZED": "അനധികൃതം",
  "INVALID_TOKEN": "അസാധുവായ ടോക്കൺ",
  "SOME_ERROR_OCCURED_IN_OFFERRIDE": "റൈഡ് ഓഫറിൽ എന്തോ പിശക് സംഭവിച്ചു",
  "SELECT_VEHICLE_TYPE": "വാഹനത്തിന്റെ തരം തിരഞ്ഞെടുക്കുക",
  "RIDE": "റൈഡ്",
  "NO_LOCATION_UPDATE": "ലൊക്കേഷൻ അപ്ഡേറ്റൊന്നുമില്ല",
  "GOT_IT_TELL_US_MORE": "ഓക്കേ, കൂടുതൽ പറയാൻ ആഗ്രഹിക്കുന്നുവോ?",
  "WRITE_A_COMMENT": " അഭിപ്രായം എഴുതുക",
  "HOW_WAS_YOUR_RIDE_WITH": "നിങ്ങളുടെ റൈഡ് എങ്ങനെ ഉണ്ടായിരുന്നു",
  "RUDE_BEHAVIOUR": "അപമര്യാദയായ പെരുമാറ്റം",
  "LONG_WAITING_TIME": "ദൈർഘ്യമേറിയ കാത്തിരിപ്പ്",
  "DIDNT_COME_TO_PICUP_LOCATION": "പിക്കപ്പ് ലൊക്കേഷനിൽ വന്നില്ല",
  "HELP_US_WITH_YOUR_REASON": "നിങ്ങളുടെ കാരണം രേഖപ്പെടുത്തി ഞങ്ങളെ സഹായിക്കൂ",
  "MAX_CHAR_LIMIT_REACHED": "പരിധിക്കപ്പുറം  അക്ഷരങ്ങൾ ആയി",
  "SHOW_ALL_OPTIONS": "എല്ലാ ഓപ്ഷനുകളും കാണിക്കുക",
  "UPDATE_REQUIRED": "അപ്ഡേറ്റ് ആവശ്യമാണ്",
  "PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE": "ഞങ്ങളുടെ ആപ്പിനായി ഒരു പുതിയ അപ്‌ഡേറ്റ് ലഭ്യമാണെന്ന് അറിയിക്കുന്നതിൽ ഞങ്ങൾക്ക് സന്തോഷമുണ്ട്. ഈ അപ്‌ഡേറ്റിൽ നിങ്ങളുടെ അനുഭവം കൂടുതൽ മികച്ചതാക്കുന്നതിന് പുതിയ ഡിസൈനും നിരവധി പുതിയ ഫീച്ചറുകളും ഉൾപ്പെടുന്നു",
  "NOT_NOW": "ഇപ്പോൾ വേണ്ട",
  "OF": "ആല്",
  "DROP": "ഡ്രോപ്പ്",
  "PLEASE_WAIT": "കാത്തിരിക്കൂ",
  "SETTING_YOU_OFFLINE": "ഞങ്ങൾ നിങ്ങളെ ഓഫ്ലൈനിൽ സജ്ജമാക്കുന്നു",
  "SETTING_YOU_ONLINE": "ഞങ്ങൾ നിങ്ങളെ ഓൺലൈനിൽ സജ്ജമാക്കുന്നു",
  "VIEW_BREAKDOWN": "വിഘടിച്ചു കാണുക",
  "APP_INFO": "അപ്ലിക്കേഷനെ കുറിച്ചുള്ള വിവരങ്ങൾ",
  "OTHER": "മറ്റുളവ",
  "VEHICLE_ISSUE": "വാഹന പ്രശ്നം",
  "FARE_UPDATED": "നിരക്ക് പുതുക്കി",
  "FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES": "പതിവ് റദ്ദാക്കലുകൾ കുറഞ്ഞ സവാരിക്കും കുറഞ്ഞ റേറ്റിംഗിലേക്കും നയിക്കും",
  "CONTINUE": "തുടരുക",
  "CONFIRM_PASSWORD": "പാസ്സ്‌വേർഡ് സ്ഥിരീകരിക്കുക",
  "DEMO_MODE": "ഡെമോ മോഡ്",
  "PASSWORD": "പാസ്സ്‌വേർഡ്",
  "ENTER_DEMO_MODE_PASSWORD": "ഡെമോ മോഡ് പാസ്സ്‌വേർഡ് നൽകുക",
  "DEMO_MODE_DISABLED": "ഡെമോ മോഡ് നിർത്തി വെച്ചിരിക്കുന്നു",
  "ONLINE_VIA_DEMO_MODE": "ഓൺലൈൻ (ഡെമോ)",
  "MORE": "കൂടുതൽ",
  "LESS": "കുറവ്",
  "YOU_ARE_AT_PICKUP": "നിങ്ങൾ പിക്കപ്പ് ലൊക്കേഷനിലാണ്",
  "WAITING_FOR_CUSTOMER": "നിങ്ങൾ ഉപഭോക്താവിനായി കാത്തിരിക്കുകയാണ്",
  "CUSTOMER_NOTIFIED": "ഉപഭോക്താവിനെ അറിയിച്ചു",
  "I_ARRIVED": "ഞാൻ എത്തി",
  "ESTIMATED_RIDE_FARE": "ഏകദേശ യാത്ര നിരക്ക്:",
  "PICKUP_TOO_FAR": "പിക്കപ്പ് വളരെ ദൂരെ",
  "CUSTOMER_NOT_PICKING_CALL": "ഉപഭോക്താവ് കോൾ എടുക്കുന്നില്ല",
  "TRAFFIC_JAM": "ഗതാഗതക്കുരുക്ക്",
  "CUSTOMER_WAS_RUDE": "ഉപഭോക്താവ് അപമര്യാദയോടെ പെരുമാറി",
  "ALERT": "അലേർട്ട്",
  "ALL_ALERTS": "എല്ലാ അലേർട്ടുകൾ",
  "ADD_A_COMMENT": "കമന്റ് രേഖപ്പെടുത്തുക",
  "POST_COMMENT": " കമന്റ് പോസ്റ്റ് ചെയ്യുക   ",
  "ENTER_YOUR_COMMENT": "കമന്റ് നൽകുക",
  "NO_NOTIFICATIONS_RIGHT_NOW": "ഇപ്പോൾ അറിയിപ്പുകളൊന്നുമില്ല!",
  "NO_NOTIFICATIONS_RIGHT_NOW_DESC": "പുതിയ അറിയിപ്പുകളുണ്ടെങ്കിൽ ഞങ്ങൾ നിങ്ങളെ അറിയിക്കും",
  "ALERTS": "അലേർട്ടുകൾ",
  "YOUR_COMMENT": "നിങ്ങളുടെ കമന്റ്",
  "SHOW_MORE": "കൂടുതൽ കാണിക്കുക",
  "LOAD_OLDER_ALERTS": "പഴയ അലേർട്ടുകൾ ലോഡു ചെയ്യുക",
  "CONTEST": "മത്സരം",
  "YOUR_REFERRAL_CODE_IS_LINKED": "നിങ്ങളുടെ റഫറൽ കോഡ് ലിങ്ക് ചെയ്യപ്പെട്ടിരിക്കുന്നു!",
  "YOU_CAN_NOW_EARN_REWARDS": "ഉപയോക്താക്കളെ റഫർ ചെയ്യുന്നതിലൂടെ നിങ്ങൾക്ക് ഇപ്പോൾ പ്രതിഫലം നേടാൻ കഴിയും!",
  "COMING_SOON": "ഉടൻ വരുന്നു!",
  "COMING_SOON_DESCRIPTION": " നിങ്ങളെ റഫറൽ പ്രോഗ്രാമിൽ പ്രവേശിപ്പിക്കുന്നതിനു ഞങ്ങൾ ശ്രമം തുടരുകയാണ്  . കൂടുതൽ വിവരങ്ങൾക്ക് അലേർട്ട്സ് പേജ് പരിശോധിക്കുക.",
  "REFERRAL_CODE": "റഫറൽ കോഡ്",
  "REFERRAL_CODE_HINT": "6 അക്ക റഫറൽ കോഡ് നൽകുക",
  "CONFIRM_REFERRAL_CODE": "റഫറൽ കോഡ് സ്ഥിരീകരിക്കുക",
  "CONFIRM_REFERRAL_CODE_HINT": "റഫറൽ കോഡ് വീണ്ടും നൽകുക",
  "YOUR_REFERRAL_CODE": "നിങ്ങളുടെ റഫറൽ കോഡ്",
  "FIRST_REFERRAL_SUCCESSFUL": "ആദ്യ റഫറൽ വിജയകരമാണ്! \n സമ്മാനം അൺലോക്കു ചെയ്തു!",
  "AWAITING_REFERRAL_RIDE": "റഫറൽ റൈഡിനായി  കാത്തിരിക്കുന്നു",
  "CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT": "റഫറൽ അലേർട്ട് കിട്ടുമ്പോൾ ഈ ഇടം സന്ദർശിക്കുക",
  "REFERRED_CUSTOMERS": "റഫർ ചെയ്‍ത ഉപഭോക്താക്കൾ ",
  "ACTIVATED_CUSTOMERS": "സജീവമാക്കിയ ഉപഭോക്താക്കൾ",
  "REFERRAL_CODE_LINKING": "റഫറൽ കോഡ് ലിങ്കു ചെയ്യുന്നു",
  "CONTACT_SUPPORT": "സപ്പോർട്ട് ടീമിനെ ബന്ധപെടുക",
  "CALL_SUPPORT": "സപ്പോർട്ട് ടീമിനെ വിളിക്കുക",
  "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "നിങ്ങൾ നമ്മ യാത്രി സപ്പോർട്ട് ടീമുമായി ബന്ധപ്പെടാൻ പോകുന്നു. തുടരണോ?",
  "REFERRAL_ENROLMENT": "റഫറൽ എൻറോൾമെന്റ്",
  "REFERRALS": "റഫറലുകൾ",
  "LINK_REFERRAL_CODE": "ലിങ്ക് റഫറൽ കോഡ്",
  "DRIVER_DETAILS": "ഡ്രൈവർ വിശദാംശങ്ങൾ",
  "FOR_UPDATES_SEE_ALERTS": "അപ്ഡേറ്റുകൾക്കായി, അലേർട്ടുകൾ കാണുക",
  "SHARE_OPTIONS": "ഷെയർ ചെയ്യുന്നതിനുള്ള ഓപ്ഷൻസ്",
  "ENTER_PASSWORD": "പാസ്സ്‌വേർഡ് നൽകുക",
  "BOOKING_OPTIONS": "ബുക്കിംഗ് ഓപ്ഷനുകൾ",
  "YOUR_VEHICLE" : "നിങ്ങളുടെ വാഹനം",
  "MAKE_YOURSELF_AVAILABLE_FOR" : "ഇതിനായി നിങ്ങളെത്തന്നെ ലഭ്യമാക്കുക"
}