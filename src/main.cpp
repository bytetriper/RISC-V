#include<cstring>
#include<cmath>
#include<cstdio>
#include<iostream>
#include<fstream>
#include<utility>
#include<bitset>
#include<queue>
#include<time.h>
using namespace std;
typedef long long ll;
typedef unsigned int ui;
#define test(x) cout<<(x);
#define TEST
#define CMDOUTPUT
class CPU
{
    public:
    ui reg[32],*mem,qreg[32];
    ui PC,IR,rd,rs1,rs2,imm;
    bool stuck,standard;
    int commandcnt;
    int jumpcnt,failcnt;
    string rev;
    string revtoma;
    string revp[4];
    int SLB_cnt,RS_cnt;
    ui get(unsigned code, unsigned high, unsigned low) {//MD?
    return code >> low & (1u << high - low + 1) - 1u;
    }
    ui Hex_DC(char buf[],int st,int en)
    {
        ui x=0;
        for(int i=st;i<=en;++i)
        {
            if(buf[i]>='A'&&buf[i]<='F')
                x=(x<<4)+buf[i]-'A'+10;
            else
                x=(x<<4)+buf[i]-'0';
        }
        return x;
    }
    enum commandtype{
        I,
        U,
        S,
        B,
        R,
        UJ,
        ERR=-1
    };
    enum command{
        LUI=0b0110111,
        AUIPC=0b0010111,
        JAL=0b1101111,
        JALR=0b1100111+(0b000<<7),
        BEQ=0b1100011+(0b000<<7),
        BNE=0b1100011+(0b001<<7),
        BLT=0b1100011+(0b100<<7),
        BGE=0b1100011+(0b101<<7),
        BLTU=0b1100011+(0b110<<7),
        BGEU=0b1100011+(0b111<<7),
        LB=0b0000011+(0b000<<7),
        LH=0b0000011+(0b001<<7),
        LW=0b0000011+(0b010<<7),
        LBU=0b0000011+(0b100<<7),
        LHU=0b0000011+(0b101<<7),
        SB=0b0100011+(0b000<<7),
        SH=0b0100011+(0b001<<7),
        SW=0b0100011+(0b010<<7),
        ADDI=0b0010011+(0b000<<7),
        SLTI=0b0010011+(0b010<<7),
        SLTIU=0b0010011+(0b011<<7),
        XORI=0b0010011+(0b100<<7),
        ORI=0b0010011+(0b110<<7),
        ANDI=0b0010011+(0b111<<7),
        SLLI=0b0010011+(0b001<<7)+(0b0000000<<10),
        SRLI=0b0010011+(0b101<<7)+(0b0000000<<10),
        SRAI=0b0010011+(0b101<<7)+(0b0100000<<10),
        ADD=0b0110011+(0b000<<7)+(0b0000000<<10),
        SUB=0b0110011+(0b000<<7)+(0b0100000<<10),
        SLL=0b0110011+(0b001<<7)+(0b0000000<<10),
        SLT=0b0110011+(0b010<<7)+(0b0000000<<10),
        SLTU=0b0110011+(0b011<<7)+(0b0000000<<10),
        XOR=0b0110011+(0b100<<7)+(0b0000000<<10),
        SRL=0b0110011+(0b101<<7)+(0b0000000<<10),
        SRA=0b0110011+(0b101<<7)+(0b0100000<<10),
        OR=0b0110011+(0b110<<7)+(0b0000000<<10),
        AND=0b0110011+(0b111<<7)+(0b0000000<<10),
        ERROR=-1
    };
    enum tomatype{
        FP,LOAD,STORE,JUMP,NONE=-1
    };
    command cmdset[38]={LUI,AUIPC,JAL,JALR,BEQ,BNE,BLT,BGE,BLTU,BGEU,LB,LH,LW,LBU,LHU,SB,SH,SW,ADDI,SLTI,SLTIU,XORI,ORI,ANDI,SLLI,SRLI,ADD,SUB,SLL,SLT,SLTU,XOR,SRL,SRA,OR,AND};
    class input
    {
        public:
        ui x;
        command cmd;
        commandtype type;
        tomatype totype;
        ui A,vj,vk,qj,qk,dest;
        bool prJump;
        int time,tag;
        bool busy,solve,occupied;
        input(int t=0):x(t),type(commandtype::ERR),cmd(command::ERROR),totype(NONE){
            A=vj=vk=qj=qk=dest=time=busy=tag=solve=occupied=prJump=0;
        }
    };
    class CPUqueue
    {
        public:
        int rear,frt;
        input q[3000];
        int siz;
        CPUqueue(int t=3):rear(0),frt(0),siz(t){
        }
        inline bool empty()
        {
            return rear==frt;
        }
        inline bool full()
        {
            return (rear+1)%siz==frt;
        }
        inline bool push(const input &x)
        {
            if(full())
            {
                cerr<<"queue full\n";
                exit(-1);
                return false;
            }
            rear=(rear+1)%siz;
            q[rear]=x;
            return true;
        }
        inline input & front()
        {
            return q[(frt+1)%siz];
        }
        inline void pop()
        {
            if(empty()){
                cerr<<"queue empty\n";
                exit(-1);
                return;
            }//MD?
            frt=(frt+1)%siz;
            q[frt]=input();
        }
    };
    CPUqueue ROB;
    CPUqueue IQ;
    class B2_Predictor
    {
        public:
        enum state{
            SN=0b00,WN=0b01,WT=0b10,ST=0b11//(S)trongly/(W)eakly (N)ot Taken/(T)aken
        };
        int st;
        B2_Predictor():st(ST){}
        void change_state(bool change)
        {
            switch(st)
            {
                case SN:if(change)st=WN; break;
                case WN:
                    if(change)
                        st=WT;
                    else
                        st=SN;
                    break;
                case WT:
                    if(change)
                        st=ST;
                    else
                        st=WN;
                    break;
                case ST:if(!change) st=WT;break;
            }
        }
        bool get_prediction()
        {
            return st>=2;
        }
        void upd(bool change)
        {
            st=(st>>1)|change;
        }
    };
    class History_Predictor
    {  
        public:
        B2_Predictor History[4];
        History_Predictor(){
            History[0].st=0;
            History[1].st=3;
            History[2].st=3;
            History[3].st=1;
        }
        void change_history_state(int pos,bool op)
        {
            History[pos].change_state(op);
        }
        bool get_prediction(int pos)
        {
            return History[pos].get_prediction();
        }
    };
    B2_Predictor branch[6];
    History_Predictor His_Pre[6];
    CPU():PC(0),IR(0),imm(0),rs1(0),rs2(0),rd(0),stuck(0),commandcnt(0),SLB_cnt(0),RS_cnt(0),rev("IUSBRJE"),revtoma("FLSJE"),IQ(32),ROB(32)
    {
        mem=new ui [2000000];
        jumpcnt=failcnt=0;
        for(int i=0;i<6;++i)
            branch[i].st=0;
        revp[0]="SN";revp[1]="WN";revp[2]="WT";revp[3]="ST";
        for(int i=0;i<32;++i)
            reg[i]=0;
        /*fstream f;
        f.open(filename,ios::in);
        if(!f.good())
        {
            printf("Open File Error");
            exit(0);
            return;
        }
        */
        char buffer[1000];
        while(~scanf("%s",&buffer))
        {
            //test(buffer);
            //test('\n');
            //if(buffer[0]=='#')
                //break;
            if(buffer[0]=='@')
            {
                PC=Hex_DC(buffer,1,8);
                //test(PC);
                //test('\n');
                continue;
            }

            int l=strlen(buffer);
            for(int i=0;i<l;i+=3)
            {    
                mem[PC++]=Hex_DC(buffer,i,i+1);
                //test(mem[PC-1]);
                //test(' ');
            }
            //test('\n');
        }
        //f.close();
        PC=0;
    }
    input RS[65];
    commandtype get_type_fromui(ui x)
    {
        if(x==0b11||x==0b0010011||x==0b0011011||x==0b1100111)
            return I;
        if(x==0b0010111||x==0b0110111)
            return U;
        if(x==0b0100011)
            return S;
        if(x==0b1100011)
            return B;
        if(x==0b0110011)
            return R;
        if(x==0b1101111)
            return UJ;
        cerr<<"Error at func[get_type_fromui]:unexpected type"<<endl;
        return ERR;
    }
    commandtype get_type(command x)
    {
        switch (x) {
            case LUI:
            case AUIPC:
                return commandtype::U;
            case JAL:
                return commandtype::UJ;
            case JALR:
            case LB:
            case LH:
            case LW:
            case LBU:
            case LHU:
            case ADDI:
            case SLTI:
            case SLTIU:
            case XORI:
            case ORI:
            case ANDI:
            case SLLI:
            case SRLI:
            case SRAI:
                return commandtype::I;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                return commandtype::B;
            case SB:
            case SH:
            case SW:
                return commandtype::S;
            case ADD:
            case SUB:
            case SLL:
            case SLT:
            case SLTU:
            case XOR:
            case SRL:
            case SRA:
            case OR:
            case AND:
                return commandtype::R;
            default:
                std::cerr << "[Error]function [get_format()] wrong with a undefined CommandType." << std::endl;
                return commandtype::ERR;
        }
    }
    tomatype get_tomatype(command x)
    {
        switch (x)
        {
            case LB:
            case LH:
            case LW:
            case LBU:
            case LHU:
                return tomatype::LOAD;
            case SB:
            case SH:
            case SW:
                return tomatype::STORE;
            case JAL:
            case JALR:
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                return tomatype::JUMP;
            default:
                return tomatype::FP;
        }
    }
    ui get_opcode(ui x)
    {
        return get(x,6,0);
    }
    ui get_rs1(ui x)
    {
        return get(x,19,15);
    }
    ui get_rs2(ui x)
    {
        return get(x,24,20);
    }
    ui get_rd(ui x)
    {
        //test("getrd\n");
        //cout<<bitset<32>(x)<<endl;
        //cout<<bitset<32>((x&((ui)0x00000f80))>>7)<<endl;
        return  get(x,11,7);
    }
    ui get_funct3(ui x)
    {
        return get(x,14,12);
    }
    ui get_funct7(ui x)
    {
        return get(x,31,25);
    }
    ui get_shamt(ui x)
    {
        return (x&(ui)0x01f00000)>>20;
    }
    ui get_immediate(ui x,commandtype type)
    {
        int ans=0,tmp=0;
        switch(type)
        {
            case commandtype::R:
                break;
             case commandtype::U:
            ans = get(x, 31, 12) << 12;
            break;
            case commandtype::UJ:
            ans =
                sign_extend(get(x, 31, 31) << 20 | get(x, 19, 12) << 12 |
                                get(x, 20, 20) << 11 | get(x, 30, 21) << 1,
                            20);
            break;
            case commandtype::I:
            ans = sign_extend(get(x, 31, 20), 11);
            break;
            case commandtype::S:
            ans = sign_extend(get(x, 31, 25) << 5 | get(x, 11, 7), 11);
            break;
            case commandtype::B:
            ans = sign_extend(get(x, 31, 31) << 12 | get(x, 7, 7) << 11 |
                                        get(x, 30, 25) << 5 | get(x, 11, 8) << 1,
                                    12);
            default:
                std::cerr << "[Error]function [get_immediate()] wrong with a undefined CommandType." << std::endl;
                break;
        }
        return ans;
    }
    ui sign_extend(unsigned x, int beg) {
    return x >> beg  ? -1 ^ (1u << beg) - 1 | x : x;
    }
    int get_reversetype(command cmd)
    {   
        switch (cmd)
        {
            case BEQ:return 0;
            case BNE:return 1;    
            case BLT:return 2;
            case BGE:return 3;
            case BLTU:return 4;
            case BGEU:return 5;
            default:
                throw 1;
        }
        return -1;
    }
    bool predictJump(const input &ip)
    {
        if(standard)
            return false;
        else
            return His_Pre[get_reversetype(ip.cmd)].get_prediction(branch[get_reversetype(ip.cmd)].st);
    }
    void printIQ()
    {
        input tmp[2000];
        int i;
        for(i=1;i;i++)
        {
            if(!IQ.empty())
            {
                tmp[i]=IQ.front();
                IQ.pop();
            }
            else
                break;
        }
        for(int j=1;j<i;j++)
        {    
            #ifndef TEST
            printf("[IN IQ]opcode:%u type:%c tomatype:%c\n",tmp[j].cmd,rev[tmp[j].type],revtoma[tmp[j].totype]);
            #endif
            IQ.push(tmp[j]);
        }
    }
    void printReg()
    {
        for(int i=0;i<32;++i)
            printf("%2d ",i);
        printf("\n");
        for(int i=0;i<32;++i)
            printf("%2u ",reg[i]);
        printf("\n");
        for(int i=0;i<32;++i)
            printf("%2u ",qreg[i]);
        printf("\n");
    }
    int ToRS(input &ip)
    {
        if(ip.totype==tomatype::LOAD||ip.totype==tomatype::STORE)
        {
            for(int i=33;i<=64;++i)
                if(!RS[i].occupied)
                {
                    RS[i]=ip;
                    ++SLB_cnt;
                    ROB.q[ip.tag].tag=i;
                    RS[i].occupied=true;
                    return i;
                }
        }
        else
        {
            for(int i=1;i<=32;++i)
                if(!RS[i].occupied)
                {
                    RS[i]=ip;
                    ++RS_cnt;
                    ROB.q[ip.tag].tag=i;
                    RS[i].occupied=true;
                    return i;
                }
        }
        return 0;
    }
    void push_toRS(input &ip)
    {
        if((ip.totype==LOAD||ip.totype==STORE))
        {
            if(SLB_cnt>=32)
            {    
                #ifndef TEST
                printf("[SLB]FULL:push error opcode:%u type:%c tomatype:%c\n",ip.cmd,rev[ip.type],rev[ip.totype]);
                #endif
                return;
            }
        }
        else
            if(RS_cnt>=32)
            {    
                #ifndef TEST
                printf("[RS]FULL:push error opcode:%u type:%c tomatype:%c\n",ip.cmd,rev[ip.type],rev[ip.totype]);
                #endif
                return;
            }
        if(ROB.full())
        {
            #ifndef TEST
                printf("[ROB]FULL:push error opcode:%u type:%c tomatype:%c\n",ip.cmd,rev[ip.type],rev[ip.totype]);
            #endif
            return;
        }
        rs1=get_rs1(ip.x);
        rs2=get_rs2(ip.x);
        switch (ip.type)
        {
            case R:   
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                else
                    ip.vj=reg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                else
                    ip.vk=reg[rs2];
                break;
            case I:
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                else
                    ip.vj=reg[rs1];
                break;
            case S:
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                else
                    ip.vj=reg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                else
                    ip.vk=reg[rs2];
                break;
            case B:
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                else
                    ip.vj=reg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                else
                    ip.vk=reg[rs2];
                break;
            case U:
            case UJ:
                break;
            default:
                cerr<<"Error at [decoding]: dicover unexpected commandtype"<<endl; 
                break;
        }
        ROB.push(ip);
        ip.tag=ROB.rear;
        ROB.q[ROB.rear].tag=ToRS(ip);
        switch (ip.type)//solve qreg[rd]
        {
            case R:
            case I:
            case U:
            case UJ:
                qreg[ip.dest]=ROB.q[ROB.rear].tag;
        }
        #ifndef TEST 
            //printf("[Correct Order]Imm:%d RS1:%u RS2:%u RD:%u\n",(int)Decode(ip.x).imm,Decode(ip.x).rs1,Decode(ip.x).rs2,Decode(ip.x).rd);//TEST
            printf("[PUSHINGRS...]PC:%x cmd:%x opcode:%u type:%c tomatype:%c qj:%u qk:%u rs1:%u rs2:%u rd:%u imm:%d\n",PC,ip.x,ip.cmd,rev[ip.type],revtoma[ip.totype],ip.qj,ip.qk,ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
        #endif
        IQ.pop();
    }
    input decode(ui x)
    {   
        //cout<<bitset<32>(ip.x)<<endl;
        input ip(x);
        ui code=get_opcode(ip.x),f3=get_funct3(ip.x),f7=get_funct7(ip.x);
        ui opcode=code;
        ip.type=get_type_fromui(code);
        if(ip.type==ERR)
        {
            ip.tag=-1;
            return ip;
        }
        switch (ip.type)
        {
            case U:
            case UJ:
                break;
            case R:
                opcode=opcode|(f7<<10);
            default:
                opcode=opcode|(f3<<7);
        }
        for(int i=0;i<37;++i)
            if(opcode==cmdset[i])
            {
                ip.cmd=cmdset[i];
                break;
            }
        ip.totype=get_tomatype(ip.cmd);
        ip.A=get_immediate(ip.x,ip.type);
        switch (ip.type)
        {
            case R:
            case U:
            case UJ:
            case I:
                ip.dest=get_rd(ip.x);
            break;
            default:
                break;
        }
        if(ip.cmd==JALR)//SP
        {   
            #ifndef TEST
            printf("JALR found: rs1:%d qj:%d\n",rs1,qreg[rs1]);//TEST
            #endif
            ip.vk=PC;
        }
        if(ip.type==U||ip.type==UJ)
            ip.vk=PC;
        if(ip.type==B)
        {    
            ip.dest=PC;
            ip.prJump=predictJump(ip);
            if(ip.prJump)//Predict jump
                PC=ip.dest+ip.A-4;
            #ifndef TEST 
            printf("[B jump FOUND]PC:%x cmd:%x opcode:%u prJump:%c\n",PC,ip.x,ip.cmd,ip.prJump?'Y':'N');//TEST
        #endif
        }
        if(ip.totype==LOAD||ip.totype==STORE)
            ip.time=3;
        else
            ip.time=1;
        
        /*-----TEST-----*/
        #ifndef TEST 
            //printf("[Correct Order]Imm:%d RS1:%u RS2:%u RD:%u\n",(int)Decode(ip.x).imm,Decode(ip.x).rs1,Decode(ip.x).rs2,Decode(ip.x).rd);//TEST
            printf("[DECODING...]PC:%x cmd:%x opcode:%u type:%c tomatype:%c\n",PC,ip.x,ip.cmd,rev[ip.type],revtoma[ip.totype]);//TEST
        #endif
        return ip;
    }
    ui EXE(input &ip)//MD
    {
        ui ans;
        switch (ip.cmd)
        {
            case LUI:
                break;
            case AUIPC:    
            case JAL:
                ip.A=ip.vk+ip.A;
                break;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                ip.A=ip.dest+ip.A;//dest==PC
                break;
            case JALR:
                ip.A=ip.vj+(ip.A&-2);
                break;
            case LB:
            case LH:   
            case LW:
            case LBU:
            case LHU:
            case SB:
            case SH:
            case SW:                                                               
            case ADDI:
                ip.A=ip.vj+ip.A;
                break;
            case SLTI:
                ip.A=(int)ip.vj<(int)ip.A;
                break;
            case SLTIU:
                ip.A=ip.vj<ip.A;
                break;
            case XORI:
                ip.A=ip.vj^ip.A;
                break;
            case ORI:
                ip.A=ip.vj|ip.A;
                break;
            case ANDI:
                ip.A=ip.vj&ip.A;
                break;
            case SLLI://SP?
                ip.A=ip.vj<<get(ip.A,4,0);
                break;
            case SRLI://SP?
                ip.A=ip.vj>>get(ip.A,4,0);
                break;
            case SRAI:
                ip.A=(int)ip.vj>>get(ip.A,4,0);
                //sign_extend(ip.A,31-get(ip.A,4,0));//CHECK?
                break;
            case ADD:
                ip.A=ip.vj+ip.vk;
                break;
            case SUB:
                ip.A=ip.vj-ip.vk;
                break;
            case SLL://SP?
                ip.A=ip.vj<<get(ip.vk,4,0);
                break;
            case SLT:
                ip.A=(int)ip.vj<(int)ip.vk;
                break;
            case SLTU:
                ip.A=ip.vj<ip.vk;
                break;
            case XOR:
                ip.A=ip.vj^ip.vk;
                break;
            case SRL:
                ip.A=ip.vj>>get(ip.vk,4,0);
                break;
            case SRA:
                ip.A=(int)ip.vj>>get(ip.vk,4,0);
                //sign_extend(ip.A,31-get(ip.vk,4,0));//CHECK?
                break;
            case OR:
                ip.A=ip.vj|ip.vk;
                break;
            case AND:
                ip.A=ip.vj&ip.vk;
                break;
            default:
                cerr<<"unexpected command at [EXE]"<<endl;
        }
        #ifndef TEST
        printf("[EXECUTING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%d\n",ip.cmd,rev[ip.type],revtoma[ip.totype],ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
        #endif
        return ans;
    }
    ui WB(const input &ip)//MD
    {
        ui ans;
        ROB.q[ip.tag].busy=true;
        ROB.q[ip.tag].A=ip.A;
        ROB.q[ip.tag].vj=ip.vj;
        ROB.q[ip.tag].vk=ip.vk;
        switch (ip.cmd)
        {
            case BEQ:
                if(ip.vj!=ip.vk)
                {
                    ROB.q[ip.tag].solve=true;//SP
                }break;
            case BNE:
                if(ip.vj==ip.vk)
                {
                    //printf("NOOOOOO\n");
                    //exit(0);
                    ROB.q[ip.tag].solve=true;//SP
                }
                break;
            case BLT:
                if((int)ip.vj>=(int)ip.vk)
                {
                    ROB.q[ip.tag].solve=true;//SP
                }break;
            case BGE:
                if((int)ip.vj<(int)ip.vk)
                {
                    ROB.q[ip.tag].solve=true;//SP
                }break;
            case BLTU:
                if(ip.vj>=ip.vk)
                {
                    ROB.q[ip.tag].solve=true;//SP
                    
                }break;
            case BGEU:
                if(ip.vj<ip.vk)
                {
                    ROB.q[ip.tag].solve=true;//SP
                    
                }break;
        }
        #ifndef TEST
            printf("[WBING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%d\n",ip.cmd,rev[ip.type],revtoma[ip.totype],ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
        #endif
        return ans;       
    }
    ui COMMIT(input &ip)//MD
    {
        ui ans;
        #ifndef TEST
            printf("[COMMITING...]opcode:%u solve:%d type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%d\n",ip.cmd,ip.solve?1:0,rev[ip.type],revtoma[ip.totype],ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
          #endif    
        #ifndef CMDOUTPUT  
          fstream f("output.data",ios::out|ios::app);
            f<<ip.cmd<<" "<<PC<<" ";
            f.close();
        #endif
        switch (ip.cmd)
        {
            case LUI:
            case AUIPC:
                reg[ip.dest]=ip.A;
                break;
            case JAL:
                reg[ip.dest]=ip.vk+4;
                PC=ip.A;
                break;
            break;
            case JALR:
                reg[ip.dest]=ip.vk+4;
                PC=ip.A;
                //printf("JAL Jumping %u %d PC:%u\n",ip.vk,(int)ip.A,PC);//TEST
                break;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                jumpcnt++;
                if(ip.solve&&ip.prJump)//Actually didn't Jump
                    PC=ip.dest+4;
                if(!ip.solve&&!ip.prJump)//Actually Jumped    
                    PC=ip.A;
                His_Pre[get_reversetype(ip.cmd)].change_history_state(branch[get_reversetype(ip.cmd)].st,!(ip.solve==ip.prJump));
                branch[get_reversetype(ip.cmd)].upd(!ip.solve);
                break;
            case LB:
                reg[ip.dest]=mem[ip.A];
                sign_extend(reg[ip.dest],7);
                break;
            case LH:
                reg[ip.dest]=mem[ip.A+1];
                reg[ip.dest]=(reg[ip.dest]<<8)|mem[ip.A];
                sign_extend(reg[ip.dest],15);
                break;    
            case LW:
                reg[ip.dest]=0;
                for(int i=3;i>=0;--i)
                    reg[ip.dest]=(reg[ip.dest]<<8)|mem[ip.A+i];
                break; 
            case LBU:
                reg[ip.dest]=mem[ip.A];
                break;
            case LHU:
                reg[ip.dest]=mem[ip.A+1];
                reg[ip.dest]=(reg[ip.dest]<<8)|mem[ip.A];
                break;
            case SB:
                mem[ip.A]=get(ip.vk,7,0);
                break;
            case SH:
                mem[ip.A+1]=get(ip.vk,15,8);
                mem[ip.A]=get(ip.vk,7,0);
                break;
            case SW:
                for(int i=3;i>=0;--i)
                    mem[ip.A+i]=get(ip.vk,8*(i+1)-1,8*i);
                break;                                                                 
            case ADDI:
            case SLTI:
            case SLTIU:
            case XORI:
            case ORI:
            case ANDI:
            case SLLI:
            case SRLI:
            case SRAI:
            case ADD:
            case SUB:
            case SLL:
            case SLT:
            case SLTU:
            case XOR:
            case SRL:
            case SRA:
            case OR:
            case AND:
                reg[ip.dest]=ip.A;
                break;
            default:
                cerr<<"unexpected command at [COMMIT]"<<endl;
        }
        return ans;
    }
    void IF()
    {
        if(stuck)return;
        ui x=0;
        for(int i=3;i>=0;--i)
            x=(x<<8)|mem[PC+i];
        input ip=decode(x);
        if(ip.tag!=-1)
            IQ.push(ip);
    }
    void printROB()
    {
        for(int i=(ROB.frt+1)%ROB.siz;i!=(ROB.rear+1)%ROB.siz;i=(i+1)%ROB.siz)
            printf("[IN ROB]number:%d time:%d solve:%d busy:%d type:%c tomatype:%c qj:%u qk:%u rd:%u imm:%d\n",ROB.q[i].tag,ROB.q[i].time,ROB.q[i].solve?1:0,ROB.q[i].busy?1:0,rev[ROB.q[i].type],revtoma[ROB.q[i].totype],ROB.q[i].qj,ROB.q[i].qk,ROB.q[i].dest,(int)ROB.q[i].A);
    }
    void broadcast(int i)
    {
        #ifndef TEST
            printf("[BROADCASTING] number:%d,opcode:%d\n",i,RS[i].cmd);//TEST
        #endif
        for(int j=1;j<=64;++j)
        {
            if(RS[j].qj==i)
            {
               
                RS[j].qj=0;
                if(RS[i].cmd!=JAL&&RS[i].cmd!=JALR)
                    RS[j].vj=RS[i].A; 
                else
                    RS[j].vj=RS[i].vk+4;
                #ifndef TEST
                    printf("dependency found on rs[%d] opcode[%d] vj[%u]\n",j,RS[j].cmd,RS[j].vj);//TEST
                #endif
            }
            if(RS[j].qk==i)
            {
                
                RS[j].qk=0;
                if(RS[i].cmd!=JAL&&RS[i].cmd!=JALR)
                    RS[j].vk=RS[i].A;
                else
                    RS[j].vk=RS[i].vk+4;
                #ifndef TEST
                    printf("dependency found on rs[%d] opcode[%d] vk[%u]\n",j,RS[j].cmd,RS[j].vk);//TEST
                #endif
            }
        }
        for(int j=33;j<=64;++j)//待加入.. LOAD对STORE的依赖
        {}
    }
    ui upd()
    {
        //printf("RS2occu:%d",RS[2].occupied?1:0);
        bool commited=0;
        stuck=IQ.full();
        if(!stuck)
            IF();
        if(!ROB.empty()&&ROB.front().busy==true)
            --ROB.front().time;
        if(!ROB.empty()&&ROB.front().busy==true&&(!ROB.front().time))//COMMIT
        {
            ++commandcnt;
            commited=1;
            if(ROB.front().x==(ui)0x0ff00513)
                return reg[10]&((ui)255);
            COMMIT(ROB.front());
            /*refresh reg dependency*/
            switch (ROB.front().type)
            {
                case R:
                case I:
                case U:
                case UJ:
                    #ifndef TEST
                        printf("[COM_dependency]rs:%u\n",ROB.front().dest);//TEST
                    #endif
                    if(qreg[ROB.front().dest]==ROB.front().tag)
                        qreg[ROB.front().dest]=0;
                    break;
            }
            reg[0]=qreg[0]=0;//SP
        }
        int broadcaster=0;
        for(int i=1;i<=64;++i)//WB
        {
            if(RS[i].solve&&(!RS[i].busy))
            {
                WB(RS[i]);
                broadcaster=i;
                RS[i].busy=true;
                break;
            }
        }
        for(int i=1;i<=64;++i)//EXE
        {
            if((!RS[i].occupied)||RS[i].solve)
                continue;
            #ifndef TEST
                printf("[QUEUE UPDATING...]number:%d type:%c tomatype:%c qj:%u qk:%u rs1:%u rs2:%u rd:%u imm:%d \n",i,rev[RS[i].type],revtoma[RS[i].totype],RS[i].qj,RS[i].qk,RS[i].vj,RS[i].vk,RS[i].dest,(int)RS[i].A);//TEST
            #endif
            if((RS[i].qj==0&&RS[i].qk==0)&&(!RS[i].busy))
                RS[i].busy=true;
            if(RS[i].busy)
            {
                EXE(RS[i]);
                RS[i].solve=true;
                RS[i].busy=false;
                continue;
            }
            
        }
        if(!IQ.empty())
            push_toRS(IQ.front());
        if(broadcaster&&broadcaster<=32)//WB BROADCAST
            broadcast(broadcaster);
        /*update cnt&broadcast*/
        if(commited)//COMMIT BROADCAST
        {
            if(ROB.front().totype==LOAD||ROB.front().totype==STORE)
            {   
                #ifndef TEST
                    printf("[broadcasting] LB-operation number:%d\n",ROB.front().tag);
                #endif
                int i=ROB.front().tag;
                for(int j=1;j<=64;++j)
                {
                    if(RS[j].qj==i)
                    {
                        RS[j].qj=0;
                        RS[j].vj=reg[ROB.front().dest];
                    }
                    if(RS[j].qk==i)
                    {
                        RS[j].qk=0;
                        RS[j].vk=reg[ROB.front().dest];
                    }
                }
                --SLB_cnt;
            }
            else
            {   
                broadcast(ROB.front().tag); 
                --RS_cnt;
            }
            RS[ROB.front().tag]=input();
            if((ROB.front().type==B&&ROB.front().solve==ROB.front().prJump)||ROB.front().cmd==JALR||ROB.front().cmd==JAL)//Not Taken
            {
                #ifndef TEST
                    printf("[PREDICT FALSE] number:%d type:%c",ROB.frt,rev[ROB.front().type]);
                #endif
                /*clear*/
                if(ROB.front().type==B)
                    failcnt++;
                while(!IQ.empty())
                    IQ.pop();
                for(int i=1;i<=64;++i)
                    RS[i]=input();
                for(int i=0;i<32;++i)
                    qreg[i]=0;
                while(!ROB.empty())
                    ROB.pop();
                RS_cnt=SLB_cnt=0;
                reg[0]=0;
                //for (int i=0;i<32;i++) printf("round %d i=%d %08x\n",commandcnt,i,reg[i]);
                #ifndef TEST
                    printROB();//TEST
                    printIQ();
                #endif
                return 256;
            }
            else
                ROB.pop();
        }
            //for (int i=0;i<32;i++) printf("round %d i=%d %08x\n",commandcnt,i,reg[i]);
        if(!stuck)
            PC+=4;
        #ifndef TEST
            printROB();//TEST
            printIQ();
        #endif
        return 256;
    }
    void print()
    {
        for(int i=0;i<20;++i)
            printf("%d ",mem[i]);
        return;
    }
    ~CPU()
    {
        delete[] mem;
    }
};

int main()
{
    srand(time(NULL));
    #ifndef CMDOUTPUT
    fstream f("output.data",ios::out|ios::trunc);
    f.close();
    //freopen("arrar_test1.out","w",stdout);
    #endif
    fclose(stderr);
    #ifndef TEST
    #endif
    //freopen("td.txt","w",stderr);
    //freopen("testcases/bulgarian.data","r",stdin);
    CPU T;
    T.standard=false;
    //freopen("testdata.data","w",stdout);
    for(int i=1;i;++i)
    {
        #ifndef TEST
            printf("[Cycle %d]\n",i);//TEST
        #endif
        int tmp=T.upd();
        if(tmp!=256)
        {    
            cout<<tmp<<endl;
            //cout<<"ANS!!!!!! "<<tmp<<endl;//TEST
            /*cout<<"fail rate:"<<(double)T.failcnt/T.jumpcnt<<endl;
            cout<<"fail/jumpcnt: "<<T.failcnt<<"/"<<T.jumpcnt<<endl;
            for(int i=0;i<6;i++)
            {
                for(int j=0;j<4;j++)
                    cout<<T.revp[T.His_Pre[i].History[j].st]<<" ";
                cout<<endl;
            }*/
            break;
        }
        //cout<<i<<endl;
        #ifndef TEST
        if(T.commandcnt%1000==0)
            printf("%d\n",T.commandcnt);
        
        //if(i%1000)
            //printf("%d\n",i);
        
            T.printReg();//TEST
            printf("[Cycle %d]end PC:%x Commandcnt:%d stuck:%c",i,T.PC,T.commandcnt,T.stuck?'Y':'N');//TEST
        #endif
    }
    //fclose(stderr);
    return 0;
}
// PC ALU
/*
P\S  True False
True  N     Y
False Y     N

*/