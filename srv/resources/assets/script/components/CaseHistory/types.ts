interface Item {
  caseId: number
  type: string
  datetime: string
  userid?: number
}

interface ActionTask {
  isChecked: boolean
  id: number
  label: string
}

interface Location {
  latitude: number,
  longitude: number,
  description: string | null
}

interface RequestBody {
  fullName: string | null
  ivsPhoneNumber: string | null
  phoneNumber: string | null
  vehicle: Vehicle | null
  location: Location | null
}

interface Vehicle {
  vin: string | null
  plateNumber: string | null
}

export interface Action extends Item {
  type: 'action'
  actiontype: string
  actionresult: string
  actioncomment: string
  serviceid?: number
  servicelabel?: string
  tasks?: ActionTask[]
}

export interface AvayaEvent extends Item {
  type: 'avayaEvent'
  aetype: string
  aeinterlocutors: string
  aecall: number
}

export interface Call extends Item {
  type: 'call'
  calltype: string
}

export interface Comment extends Item {
  type: 'comment'
  commenttext: string
}

export interface PartnerCancel extends Item {
  type: 'partnerCancel'
  partnername: string
  refusalcomment: string
  refusalreason: string
}

export interface PartnerDelay extends Item {
  type: 'partnerDelay'
  serviceid: number
  servicelabel: string
  partnername: string
  delayminutes: string
  delayconfirmed: string
}

export interface SmsForPartner extends Item {
  type: 'smsForPartner'
  deliverystatus: string
  msgtext: string
  phone: string
  mtime: string | null
}

export interface LocationSharingResponse extends Item {
  type: 'locationSharingResponse'
  lat: number
  lon: number
  accuracy: number
}

export interface LocationSharingRequest extends Item {
  type: 'locationSharingRequest'
  smsSent: boolean
}

export interface CustomerFeedback extends Item {
  type: 'customerFeedback'
  eventType: 'FeedbackRequested' | 'FeedbackReceived'
  data: {
    operValue?: number
    techValue?: number
    comment?: number
  }
}

export interface EraGlonassIncomingCallCard extends Item {
  type: 'eraGlonassIncomingCallCard'
  requestId: number
  requestBody: RequestBody
}

type ItemData =
    Action
  | AvayaEvent
  | Call
  | Comment
  | PartnerCancel
  | PartnerDelay
  | SmsForPartner
  | LocationSharingResponse
  | LocationSharingRequest
  | CustomerFeedback
  | EraGlonassIncomingCallCard

export type HistoryItem = {
  timestamp: string
  who: string
  color: string | null
  data: ItemData
}
