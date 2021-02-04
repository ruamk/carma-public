import {h, render, FunctionalComponent } from "preact"
import {useState} from "preact/hooks"
import moment from "moment"

import {CaseFeedback} from "./CaseFeedback"
import {ServiceFeedback} from "./ServiceFeedback"
import {ModalWrapper} from "./Modal"
import {Spinner} from "./Spinner"


const GENERIC_ERROR = new Error("Что-то пошло не так");

async function loadFeedback(caseId, serviceId) {
  const r = await fetch(
    "/customerFeedback?" + new URLSearchParams({caseId, serviceId})
  );
  if(!r.ok) throw GENERIC_ERROR;
  else return r.json();
}


function mkFeedbackSaver(caseId, serviceId) {
  return async (response) => {
    const r = await fetch("/customerFeedback", {
      method: "POST",
      headers: {
        "Accept": "application/json",
        "Content-Type": "application/json"
      },
      body: JSON.stringify({caseId, serviceId, response})
    });

    if(!r.ok) throw GENERIC_ERROR;
    else return r.json();
  };
}


export async function show(
  caseId: number,
  serviceId: number | null,
  readonly: boolean // FIXME: readonly forms
) {
  const container = document.getElementById("injected-modal");
  const destroy = () => render(null, container);

  // FIXME: cancel loadFeedback in onClose
  const spinner = <ModalWrapper onClose={destroy}>
    <Spinner /> Загрузка...
  </ModalWrapper>;

  render(spinner, container);

  const fb = await loadFeedback(caseId, serviceId);
  // FIXME: catch err and show it

  const dialog = serviceId
    ? <ServiceFeedback
        readonly={readonly}
        data={fb.length > 0 ? fb[0] : null}
        onClose={destroy}
        onSave={mkFeedbackSaver(caseId, serviceId)}/>
    : <CaseFeedback
        readonly={readonly}
        data={fb.length > 0 ? fb[0] : null}
        onClose={destroy}
        onSave={mkFeedbackSaver(caseId, serviceId)}/>;

  render(dialog, container);
}
