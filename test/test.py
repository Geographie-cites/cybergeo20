from selenium import webdriver
from selenium.webdriver.common import action_chains
import time
import sys

#driver = webdriver.Firefox()

profile=webdriver.FirefoxProfile()
profile.set_preference('network.proxy.type', 1)
profile.set_preference("network.proxy.socks", "127.0.0.1")
profile.set_preference("network.proxy.socks_port",int(sys.argv[1]))
profile.set_preference("network.proxy.socks_version", 5)
profile.update_preferences()

driver = webdriver.Firefox(firefox_profile=profile)

#driver.get('http://shiny.parisgeo.cnrs.fr/CybergeoNetworks/')
#driver.get("http://api.ipify.org")
#print(driver.page_source)

#driver.close()

for i in range(20):
    print(i)
    driver.get('http://shiny.parisgeo.cnrs.fr/CybergeoNetworks/')
    #element.get_attribute('innerHTML')
    #time.sleep(10)
    #b1 = driver.find_element_by_css_selector("#tab-8338-5")
    b1=driver.find_elements_by_link_text("Citation network")[0]
    b2 =driver.find_elements_by_link_text("Geo-semantic Networks")[0]
    print(b1)

    action_chains.ActionChains(driver).click(b1).perform()
    time.sleep(5)
    action_chains.ActionChains(driver).click(b2).perform()
    time.sleep(5)
    #actions = action_chains.ActionChains(driver)
    #actions.move_to_element(b1)
    #actions.click(b2)
    #actions.perform()
    #time.sleep(10)

driver.close()
