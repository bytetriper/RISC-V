#include<cstring>
#include<cmath>
#include<cstdio>
#include<iostream>
#include<fstream>
#include<utility>
#include<bitset>
#include<queue>
#include "TA_riscv.cpp"
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
    bool stuck;
    bool pcstuck;
    int commandcnt;
    int jumpcnt,failcnt;
    string rev;
    string revtoma;
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
        tomatype tomatype;
        ui A,vj,vk,qj,qk,dest;
        int time,tag;
        bool busy,solve,occupied;
        bool predictjump;
        input(int t=0):x(t),type(commandtype::ERR),cmd(command::ERROR),tomatype(NONE){
            A=vj=vk=qj=qk=dest=time=busy=tag=solve=occupied=predictjump=0;
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
                return;
            }//MD?
            frt=(frt+1)%siz;
            q[frt]=input();
        }
    };
    CPUqueue ROB;
    CPUqueue IQ;
    CPU(string filename):PC(0),IR(0),imm(0),rs1(0),rs2(0),rd(0),pcstuck(0),stuck(0),commandcnt(0),SLB_cnt(0),RS_cnt(0),rev("IUSBRJE"),revtoma("FLSJE"),IQ(50),ROB(50),jumpcnt(0),failcnt(0)
    {
        mem=new ui [2000000];
        for(int i=0;i<32;++i)
            reg[i]=0;
        fstream f;
        f.open(filename,ios::in);
        if(!f.good())
        {
            printf("Open File Error");
            exit(0);
            return;
        }
        char buffer[1000];
        while(f.getline(buffer,100))
        {
            test(buffer);
            test('\n');
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
        f.close();
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
            printf("[IN IQ]opcode:%u type:%c tomatype:%c\n",tmp[j].cmd,rev[tmp[j].type],revtoma[tmp[j].tomatype]);
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
        if(ip.tomatype==tomatype::LOAD||ip.tomatype==tomatype::STORE)
        {
            for(int i=33;i<=64;++i)
                if(!RS[i].occupied)
                {
                    RS[i]=ip;
                    ++SLB_cnt;
                    ROB.q[ip.tag].tag=i;
                    qreg[ip.dest]=i;
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
                    qreg[ip.dest]=i;
                    RS[i].occupied=true;
                    return i;
                }
        }
        return 0;
    }
    inline void push_toRS(input &ip)
    {
        if((ip.tomatype==LOAD||ip.tomatype==STORE))
        {
            if(SLB_cnt>=32)
            {    
                #ifndef TEST
                printf("[SLB]FULL:push error opcode:%u type:%c tomatype:%c\n",ip.cmd,rev[ip.type],rev[ip.tomatype]);
                #endif
                return;
            }
        }
        else
            if(RS_cnt>=32)
            {    
                #ifndef TEST
                printf("[RS]FULL:push error opcode:%u type:%c tomatype:%c\n",ip.cmd,rev[ip.type],rev[ip.tomatype]);
                #endif
                return;
            }
        if(ROB.full())
        {
            #ifndef TEST
                printf("[ROB]FULL:push error opcode:%u type:%c tomatype:%c\n",ip.cmd,rev[ip.type],rev[ip.tomatype]);
            #endif
            return;
        }
        rd=get_rd(ip.x);
        rs1=get_rs1(ip.x);
        rs2=get_rs2(ip.x);
        imm=get_immediate(ip.x,ip.type);
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
                ip.dest=rd;
                break;
            case I:
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                else
                    ip.vj=reg[rs1];
                ip.A=imm;
                ip.dest=rd;
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
                ip.A=imm;
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
                ip.A=imm;//SP?
                break;
            case U:
            case UJ:
                ip.A=imm;
                ip.dest=rd;
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
            printf("[PUSHINGRS...]PC:%x cmd:%x opcode:%u type:%c tomatype:%c qj:%u qk:%u rs1:%u rs2:%u rd:%u imm:%d\n",PC,ip.x,ip.cmd,rev[ip.type],revtoma[ip.tomatype],ip.qj,ip.qk,ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
        #endif
        IQ.pop();
    }
    bool predict_Jump(input &ip)
    {
        if(ip.cmd==JAL||ip.cmd==JALR)
            return true;
        return true;
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
        ip.tomatype=get_tomatype(ip.cmd);
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
            ip.dest=PC;
        if(ip.tomatype==LOAD||ip.tomatype==STORE)
            ip.time=3;
        else
            ip.time=1;
        if(ip.tomatype==JUMP)
        {    
            ip.predictjump=predict_Jump(ip);
            #ifndef TEST
                printf("[DECODING] B Jump FIND prediction:%c\n",ip.predictjump?'Y':'N');
            #endif
            if(ip.predictjump)
                pcstuck=1;
        }
        /*-----TEST-----*/
        #ifndef TEST 
            //printf("[Correct Order]Imm:%d RS1:%u RS2:%u RD:%u\n",(int)Decode(ip.x).imm,Decode(ip.x).rs1,Decode(ip.x).rs2,Decode(ip.x).rd);//TEST
            printf("[DECODING...]PC:%x cmd:%x opcode:%u type:%c tomatype:%c\n",PC,ip.x,ip.cmd,rev[ip.type],revtoma[ip.tomatype]);//TEST
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
                if(ip.predictjump)
                {
                    PC=ip.A-4;
                    pcstuck=false;
                }
                break;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                ip.A=ip.dest+ip.A;//dest==PC
                if(ip.predictjump)
                {
                    PC=ip.A-4;
                    pcstuck=false;
                }
                break;
            case JALR:
                ip.A=ip.vj+(ip.A&-2);
                if(ip.predictjump)
                {
                    PC=ip.A-4;
                    pcstuck=false;
                }
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
        printf("[EXECUTING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%d\n",ip.cmd,rev[ip.type],revtoma[ip.tomatype],ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
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
            printf("[WBING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%d\n",ip.cmd,rev[ip.type],revtoma[ip.tomatype],ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
        #endif
        return ans;       
    }
    ui COMMIT(input &ip)//MD
    {
        ui ans;
        #ifndef TEST
            printf("[COMMITING...]opcode:%u solve:%d type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%d\n",ip.cmd,ip.solve?1:0,rev[ip.type],revtoma[ip.tomatype],ip.vj,ip.vk,ip.dest,(int)ip.A);//TEST
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
            case JALR:
                reg[ip.dest]=ip.vk+4;
                //printf("JAL Jumping %u %d PC:%u\n",ip.vk,(int)ip.A,PC);//TEST
                break;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
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
            printf("[IN ROB]number:%d time:%d solve:%d busy:%d type:%c tomatype:%c qj:%u qk:%u rd:%u imm:%d\n",ROB.q[i].tag,ROB.q[i].time,ROB.q[i].solve?1:0,ROB.q[i].busy?1:0,rev[ROB.q[i].type],revtoma[ROB.q[i].tomatype],ROB.q[i].qj,ROB.q[i].qk,ROB.q[i].dest,(int)ROB.q[i].A);
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
        //printf("RS2occu:%d",RS[2].occupied?1:0)
        bool commited=0;
        if(IQ.full())
            stuck=1;
        else
            stuck=0;
        if(!stuck&&!pcstuck)
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
            if(ROB.front().tomatype==JUMP)
                ++jumpcnt;
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
                printf("[QUEUE UPDATING...]number:%d type:%c tomatype:%c qj:%u qk:%u rs1:%u rs2:%u rd:%u imm:%d \n",i,rev[RS[i].type],revtoma[RS[i].tomatype],RS[i].qj,RS[i].qk,RS[i].vj,RS[i].vk,RS[i].dest,(int)RS[i].A);//TEST
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
            if(ROB.front().tomatype==LOAD||ROB.front().tomatype==STORE)
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
            if(ROB.front().tomatype==JUMP)
            {
                /*clear*/
                if((ROB.front().solve==ROB.front().predictjump))//Not taken
                {    
                    ++failcnt;
                    if(ROB.front().type==B)//b jump
                    {
                        if(!ROB.front().solve&&!ROB.front().predictjump) 
                            PC=ROB.front().A;
                        if(ROB.front().solve&&ROB.front().predictjump)
                            PC=ROB.front().dest+4;
                    }
                    else
                        PC=ROB.front().A;//jal or jalr
                    while(!IQ.empty())
                        IQ.pop();
                    for(int i=0;i<64;++i)
                        RS[i]=input();
                    for(int i=0;i<32;++i)
                        qreg[i]=0;
                    while(!ROB.empty())
                        ROB.pop();
                    RS_cnt=SLB_cnt=0;
                    pcstuck=stuck=0;
                    reg[0]=0;
                    //for (int i=0;i<32;i++) printf("round %d i=%d %08x\n",commandcnt,i,reg[i]);
                    return 256;
                }
                else//taken
                {
                    ROB.pop();
                    #ifndef TEST
                        printROB();//TEST
                        printIQ();
                    #endif
                }
            }
            else
            {    
                ROB.pop();
            }
        }
            //for (int i=0;i<32;i++) printf("round %d i=%d %08x\n",commandcnt,i,reg[i]);
        if(!stuck&&!pcstuck)
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
    #ifndef CMDOUTPUT
    fstream f("output.data",ios::out|ios::trunc);
    f.close();
    //freopen("arrar_test1.out","w",stdout);
    #endif
    fclose(stderr);
    #ifndef TEST
    #endif
    //freopen("td.txt","w",stderr);
    CPU T("testcases/expr.data");
    for(int i=1;i;++i)
    {
        #ifndef TEST
            printf("[Cycle %d]\n",i);//TEST
        #endif
        int tmp=T.upd();
        if(tmp!=256)
        {    
            cout<<"ANS!!!!!! "<<tmp<<endl;//TEST
            cout<<"fail rate:"<<(double)T.failcnt/T.jumpcnt<<endl;
            break;
        }
        cout<<i<<endl;
        if(T.commandcnt%1000==0)
            printf("%d\n",T.commandcnt);
        #ifndef TEST
        //if(i%1000)
            //printf("%d\n",i);
        
            T.printReg();//TEST
            printf("[Cycle %d]end PC:%x Commandcnt:%d stuck:%c PCstuck:%c\n",i,T.PC,T.commandcnt,T.stuck?'Y':'N',T.pcstuck?'Y':'N');//TEST
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