# CPI TRACE 🛠️

**Trace mode** is an essential debugging tool in SAP Cloud Platform Integration (CPI). It allows you to inspect the message payload and its transformations at each step (or "shape") within your integration flow (iFlow). This is invaluable for troubleshooting, verifying your logic, and understanding how data is processed.

---

## Recommended Tool: CPI Helper Extension 🚀

While you can manage trace settings manually, the **CPI Helper Chrome Extension** significantly streamlines the process. It's a highly recommended tool for any CPI developer that adds a helpful toolbar directly into the iFlow editor.

* **Install Extension:** [CPI Helper on Chrome Web Store](https://chromewebstore.google.com/detail/sap-cpi-helper/epoggeaemnkacpinjfgccbjakglngkpb)
* **Official Documentation:** [CPI Helper on GitHub](https://github.com/dbeck121/CPI-Helper-Chrome-Extension)

---

## How to Activate Trace

Once the extension is installed, reload the page. When you access your integrations, a new set of buttons will appear.

Follow the steps shown in the image below:


<img width="583" height="469" alt="image" src="https://github.com/user-attachments/assets/662f533e-083c-414f-bd80-2731c4274e96" />

1.  **Enable Trace Mode:** Click the **Trace** button to activate it for the next run.
2.  **Deploy & Run:** **Deploy** and then trigger your integration to process a message.
3.  **View the Trace:** Click the **Trace** button to inspect the last request.

---

## Further Learning

For a deeper dive, please refer to SAP's official learning path.

* **SAP BTP Mission:** [Build an Integration Scenario with an External Non-SAP System](https://developers.sap.com/mission.btp-integration-suite-nonsapconnectivity.html)
